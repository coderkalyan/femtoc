const std = @import("std");
const hir = @import ("hir.zig");
const ast = @import("ast.zig");
const lex = @import("lex.zig");
const parse = @import("parse.zig");
const parseInt = @import("integerLiteral.zig").parseInt;
const parseFloat = @import("floatLiteral.zig").parseFloat;

const Allocator = std.mem.Allocator;
const Hir = hir.Hir;
const Inst = hir.Inst;
const Ast = ast.Ast;
const Node = ast.Node;
const Token = lex.Token;

pub const GenError = error { UnexpectedToken, InvalidIdentifier, InvalidRef };

pub fn generate(gpa: Allocator, tree: *const Ast) !Hir {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var hirgen = HirGen {
        .gpa = gpa,
        .arena = arena.allocator(),
        .tree = tree,
        .inst = std.MultiArrayList(Inst){},
        .extra_data = .{},
        .strings = .{},
        .string_table = .{},
    };

    var toplevel = HirGen.Scope.Toplevel {
        .base = .{
            .tag = .toplevel,
        }
    };
    try hirgen.constDecl(&toplevel.base, 0);

    return Hir {
        .inst = hirgen.inst.toOwnedSlice(),
        .extra_data = hirgen.extra_data.toOwnedSlice(gpa),
    };
}

pub const HirGen = struct {
    gpa: Allocator,
    arena: Allocator,

    tree: *const Ast,
    inst: std.MultiArrayList(Inst),
    extra_data: std.ArrayListUnmanaged(Inst.Ref),
    scratch: std.ArrayListUnmanaged(Inst.Ref),
    // strings: std.StringArrayHashMapUnmanaged(void),
    strings: std.ArrayListUnmanaged([]const u8),
    string_table: std.StringHashMapUnmanaged(u32),

    fn parseIntToken(hg: *HirGen, index: ast.TokenIndex) !u64 {
        const source = hg.tree.source;
        const start = hg.tree.tokens.items(.start)[@intCast(u32, index)];
        var lexer = lex.Lexer {
            .buffer = source,
            .index = start,
        };
        const token = lexer.next();
        std.debug.assert(token.tag == .int_lit);
        return parseInt(source[start..token.loc.end]);
    }

    fn parseFloatToken(hg: *HirGen, index: ast.TokenIndex) !f64 {
        const source = hg.tree.source;
        const start = hg.tree.tokens.items(.start)[@intCast(u32, index)];
        var lexer = lex.Lexer {
            .buffer = source,
            .index = start,
        };
        const token = lexer.next();
        std.debug.assert(token.tag == .float_lit);
        return parseFloat(source[token.loc.start..token.loc.end]);
    }

    fn parseNumberToken(hg: *HirGen, index: Token.Index) !union { int: u64, float: f64 } {
        const source = hg.tree.source;
        const start = hg.tree.tokens.items(.start)[@intCast(u32, index)];
        var lexer = lex.Lexer {
            .buffer = source,
            .index = start,
            .pending_invalid_token = null,
        };
        const token = lexer.next();
        return switch (token.tag) {
            .int_lit => parseInt(source[start..token.loc.end]),
            .float_lit => parseFloat(source[start..token.loc.end]),
            else => GenError.UnexpectedToken,
        };
    }

    fn addInst(hg: *HirGen, inst: Inst) !Inst.Index {
        const result = @intCast(Inst.Index, hg.inst.len);
        try hg.inst.append(hg.gpa, inst);
        return result;
    }

    fn indexToRef(index: Inst.Index) Inst.ValRef {
        const ref_len = @intCast(u32, @typeInfo(Inst.ValRef).Enum.fields.len);
        return @intToEnum(Inst.ValRef, ref_len + index);
    }

    fn refToIndex(ref: Inst.ValRef) !Inst.Index {
        const ref_len = @intCast(u64, @typeInfo(Inst.ValRef).Enum.fields.len);
        const index = @enumToInt(ref);
        return if (index >= ref_len) index - ref_len else GenError.InvalidRef;
    }

    fn addExtra(hg: *HirGen, extra: anytype) Allocator.Error!Inst.Index {
        const fields = std.meta.fields(@TypeOf(extra));
        try hg.extra_data.ensureUnusedCapacity(hg.gpa, fields.len);
        const len = @intCast(u32, hg.extra_data.items.len);
        inline for (fields) |field| {
            if (field.field_type == Inst.ValRef) {
                hg.extra_data.appendAssumeCapacity(.{ .val = @field(extra, field.name) });
            } else if (field.field_type == Inst.TyRef) {
                hg.extra_data.appendAssumeCapacity(.{ .val = @field(extra, field.name) });
            } else if (field.field_type == u64) {
                hg.extra_data.appendAssumeCapacity(.{ .str = @field(extra, field.name) });
            } else {
                unreachable;
            }
        }
        return len;
    }

    fn putString(hg: *HirGen, str: []const u8) !u32 {
        const index = @intCast(u32, hg.strings.items.len);
        try hg.strings.append(hg.gpa, str);
        try hg.string_table.putNoClobber(hg.gpa, str, index);
        return index;
    }

    fn getStringIndex(hg: *HirGen, str: []const u8) !u32 {
        return hg.string_table.get(str) orelse GenError.InvalidRef;
    }

    fn integerLiteral(hg: *HirGen, index: Node.Index) !Inst.ValRef {
        const nodes = hg.tree.nodes;
        const int_token = nodes.items(.main_token)[index];
        const value = hg.parseIntToken(int_token) catch return GenError.UnexpectedToken;
        const result: Inst.ValRef = switch (value) {
            0 => .zero,
            1 => .one,
            else => HirGen.indexToRef(try hg.addInst(.{
                .tag = .int,
                .data = .{ .int = value },
            })),
        };
        return result;
    }

    fn floatLiteral(hg: *HirGen, index: Node.Index) !Inst.ValRef {
        const nodes = hg.tree.nodes;
        const float_token = nodes.items(.main_token)[index];
        const value = hg.parseFloatToken(float_token) catch return GenError.UnexpectedToken;
        return HirGen.indexToRef(try hg.addInst(.{
            .tag = .float,
            .data = .{ .float = value },
        }));
    }

    fn identifier(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.ValRef {
        const nodes = hg.tree.nodes;
        const tokens = hg.tree.tokens;
        const ident_index = nodes.items(.main_token)[index];
        const ident_start = tokens.items(.start)[ident_index];
        var lexer = lex.Lexer {
            .buffer = hg.tree.source,
            .index = ident_start,
            .pending_invalid_token = null,
        };
        const token = lexer.next();
        const token_str = hg.tree.source[token.loc.start..token.loc.end];
        const uniq = try hg.getStringIndex(token_str);
        
        const src = Scope.resolveVar(scope, uniq);
        if (src) |inst| {
            return HirGen.indexToRef(inst);
        } else {
            return GenError.InvalidIdentifier;
        }
    }

    fn call(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.ValRef {
        const nodes = hg.tree.nodes;
        const tokens = hg.tree.tokens;
        const ident_index = nodes.items(.main_token)[index];
        const ident_start = tokens.items(.start)[ident_index];
        var lexer = lex.Lexer {
            .buffer = hg.tree.source,
            .index = ident_start,
            .pending_invalid_token = null,
        };
        const token = lexer.next();
        const token_str = hg.tree.source[token.loc.start..token.loc.end];
        const uniq = try hg.getStringIndex(token_str);

        const src = Scope.resolveVar(scope, uniq);
        const ref = if (src) |inst| ref: {
            break :ref HirGen.indexToRef(inst);
            // break :ref @typeInfo(Inst.ValRef).Enum.fields.len + inst;
        } else {
            return GenError.InvalidIdentifier;
        };

        const data = nodes.items(.data)[index];
        const proto = hg.tree.extraData(data.l, Node.FnProto);
        const args_start = @intCast(u32, hg.extra_data.items.len);
        var param_index = proto.params_start;
        while (param_index <= proto.params_end) : (param_index += 1) {
            _ = try hg.addExtra(Inst.Arg {
                .val = try hg.expr(scope, param_index),
            });
        }
        var args_end = @intCast(u32, hg.extra_data.items.len) - 1;
        
        return HirGen.indexToRef(try hg.addInst(.{
            .tag = .call,
            .data = .{
                .extra_index = try hg.addExtra(Inst.Call {
                    .addr = ref,
                    .args_start = HirGen.indexToRef(args_start),
                    .args_end = HirGen.indexToRef(args_end),
                }),
            },
        }));
    }

    fn fnDecl(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.ValRef {
        const nodes = hg.tree.nodes;
        const tokens = hg.tree.tokens;
        const ident_index = nodes.items(.main_token)[index];
        const ident_start = tokens.items(.start)[ident_index];
        var lexer = lex.Lexer {
            .buffer = hg.tree.source,
            .index = ident_start,
            .pending_invalid_token = null,
        };
        const token = lexer.next();
        const token_str = hg.tree.source[token.loc.start..token.loc.end];
        const uniq = try hg.getStringIndex(token_str);

        const data = nodes.items(.data)[index];
        const proto = hg.tree.extraData(data.l, Node.FnProto);

        const scratch_top = hg.scratch.items.len;
        defer hg.scratch.shrinkRetainingCapacity(scratch_top);

        var proto_param_index = proto.params_start;
        while (proto_param_index <= proto.params_end) : (proto_param_index += 1) {
            const params;
        }

        const params_start = @intCast(u32, hg.extra_data.items.len);
        var param_index = proto.params_start;
        while (param_index <= proto.params_end) : (param_index += 1) {
            _ = try hg.addExtra(Inst.Arg {
                .val = try hg.expr(scope, param_index),
            });
        }
        var params_end = @intCast(u32, hg.extra_data.items.len) - 1;
        
        return HirGen.indexToRef(try hg.addInst(.{
            .tag = .call,
            .data = .{
                .extra_index = try hg.addExtra(Inst.Call {
                    .addr = ref,
                    .params_start = HirGen.indexToRef(args_start),
                    .params_end = HirGen.indexToRef(args_end),
                }),
            },
        }));
    }

    fn binary(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.ValRef {
        const nodes = hg.tree.nodes;
        // const main_token = nodes.items(.main_token)[index];
        const data = nodes.items(.data)[index];

        const lref = try hg.expr(scope, data.l);
        const rref = try hg.expr(scope, data.r);
        const node_tag = nodes.items(.tag)[index];
        const tag = switch (node_tag) {
            Node.Tag.add => Inst.Tag.add,
            Node.Tag.sub => Inst.Tag.sub,
            Node.Tag.mul => Inst.Tag.mul,
            Node.Tag.div => Inst.Tag.div,
            else => return GenError.UnexpectedToken,
        };

        return HirGen.indexToRef(try hg.addInst(.{
            .tag = tag,
            .data = .{ 
                .extra_index = try hg.addExtra(Inst.Binary {
                    .lref = lref,
                    .rref = rref,
                }),
            },
        }));
    }

    fn expr(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.ValRef {
        const nodes = hg.tree.nodes;
        return switch (nodes.items(.tag)[index]) {
            .int_lit => hg.integerLiteral(index),
            .float_lit => hg.floatLiteral(index),
            .ident_expr => hg.identifier(scope, index),
            .call_expr => hg.call(scope, index),
            .add, .sub, .mul, .div => hg.binary(scope, index),
            .fn_decl => hg.fnDecl(scope, index),
            else => unreachable,
            // else => @intToEnum(Inst.ValRef, 0),
        };
    }

    fn constDecl(hg: *HirGen, scope: *Scope, index: Node.Index) !void {
        const nodes = hg.tree.nodes;
        const tokens = hg.tree.tokens;

        const ident_index = nodes.items(.main_token)[index] + 1;
        const ident_start = tokens.items(.start)[ident_index];
        var lexer = lex.Lexer {
            .buffer = hg.tree.source,
            .index = ident_start,
            .pending_invalid_token = null,
        };
        const token = lexer.next();
        const token_str = hg.tree.source[token.loc.start..token.loc.end];

        const uniq = try hg.putString(token_str);

        // TODO: type annotations
        const data = nodes.items(.data)[index];
        const ref = try hg.expr(scope, data.l);
        const block = scope.cast(Scope.Block).?;
        std.debug.print("token={s} uniq={} ref={}\n", .{token_str, uniq, ref});
        try block.decls.put(hg.arena, uniq, ref);
    }

    fn ifStmt(hg: *HirGen, scope: *Scope, index: Node.Index) void {
        const nodes = hg.tree.nodes;
        const data = nodes.items(.data)[index];

        const cond_ref = hg.expr(scope, data.l);
        const block_ref = hg.block(scope, data.r);

        return hg.addInst(.{
            .tag = .branch,
            .data = .{
                .extra_index = hg.addExtra(.{
                    .cond = cond_ref,
                    .addr = block_ref,
                }),
            },
        });
    }

    const Scope = struct {
        tag: Tag,

        fn cast(base: *Scope, comptime T: type) ?*T {
            return @fieldParentPtr(T, "base", base);
        }

        fn parent(base: *Scope) ?*Scope {
            return switch (base.tag) {
                .toplevel => null,
                .namesapce => base.cast(Namespace).?.parent,
                .block => base.cast(Block).?.parent,
            };
        }

        const Tag = enum {
            toplevel,
            namespace,
            block,
        };

        const Toplevel = struct {
            const base_tag: Tag = .toplevel;
            base: Scope = .{ .tag = base_tag },
        };
        const Namespace = struct {
            const base_tag: Tag = .namespace;
            base: Scope = .{ .tag = base_tag },

            parent: *Scope,
            decls: std.AutoArrayHashMapUnmanaged(u32, Inst.ValRef),
        };
        const Block = struct {
            const base_tag: Tag = .block;
            base: Scope = .{ .tag = base_tag },
            base_inst: Inst.Index,

            parent: *Scope,
            decls: std.AutoArrayHashMapUnmanaged(u32, Inst.ValRef),
        };

        fn resolveVar(scope: *Scope, ident: u32) ?Inst.Index {
            var found: ?Inst.Index = null;
            var s: *Scope = scope;

            while (true) {
                switch (s.tag) {
                    .toplevel => break,
                    .namespace => {
                        const namespace = s.cast(Scope.Namespace).?;
                        const index = namespace.decls.get(ident);
                        if (index) |loc| {
                            found = HirGen.refToIndex(loc) catch unreachable;
                        }
                    },
                    .block => {
                        // TODO: order and rebinding
                        const block = scope.cast(Block).?;
                        const index = block.decls.get(ident);
                        if (index) |loc| {
                            found = HirGen.refToIndex(loc) catch unreachable;
                        }
                    },
                }
            }

            return found;
        }
    };
};
