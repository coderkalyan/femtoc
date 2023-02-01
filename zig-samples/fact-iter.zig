export fn fact(n: i32) i32 {
    var ans: i32 = 1;
    var i: i32 = 1;
    while (i <= n) : (i += 1) {
        ans *= i;
    }

    return ans;
}
