const std = @import("std");
const Type = @import("type.zig").Type;
const InternPool = @import("../InternPool.zig");

// peer type resolution tries to find a common destination type that a set of source types
// can coerce into
fn peerType(pool: *InternPool, src: Type, dest: Type) ?Type {
    _ = pool;
    const src_tag: Type.Tag = src;
    const dest_tag: Type.Tag = dest;

    // both comptime ints (make sure sign matches)
    if (src_tag == .comptime_int and dest_tag == .comptime_int) {
        if (src.comptime_int.sign != dest.comptime_int.sign) return null;
        return src;
    }

    // comptime int and fixed int
    if (src_tag == .comptime_int and dest_tag == .int) {
        if (src.comptime_int.sign == .signed and dest.int.sign == .unsigned) return null;
        return dest;
    }

    // both fixed ints (make sure sign matches, and dest is at least as large)
    if (src_tag == .int and dest_tag == .int) {
        if (src.int.sign != dest.int.sign) return null;
        if (src.int.width > dest.int.width) return null;
        return dest;
    }

    // both comptime floats
    if (src_tag == .comptime_float and dest_tag == .comptime_float) {
        return src;
    }

    // comptime float and fixed float
    if (src_tag == .comptime_float and dest_tag == .float) {
        return dest;
    }

    // both fixed floats (make sure dest is at least as large)
    if (src_tag == .float and dest_tag == .float) {
        if (src.float.width > dest.float.width) return null;
        return dest;
    }

    return null;
}

fn resolvePeerTypesInner(pool: *InternPool, a: Type, b: Type) ?Type {
    // try resolving the types both ways
    return peerType(pool, a, b) orelse peerType(pool, b, a);
}

pub fn resolvePeerTypes(pool: *InternPool, types: []const Type) ?Type {
    // pairwise "reduce" the list of types to a single peer type
    // if any reduction step fails, the entire set fails
    if (types.len == 1) return types[0];

    var peer_type = types[0];
    for (types[1..]) |src_type| {
        peer_type = resolvePeerTypesInner(pool, peer_type, src_type) orelse return null;
    }
    return peer_type;
}
