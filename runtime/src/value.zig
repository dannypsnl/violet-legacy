const std = @import("std");
const testing = std.testing;

const Rigid = struct { head: u64, spine: []VioletValue };
const Pair = struct { left: *VioletValue, right: *VioletValue };
const VioletValue = union(enum) {
    typ: void,
    rigid: Rigid,
    pair: Pair,
    lam: void,
};

test "basic add functionality" {
    var a = VioletValue{ .rigid = Rigid{ .head = 1, .spine = &[_]VioletValue{} } };
    var b = VioletValue{ .rigid = Rigid{ .head = 1, .spine = &[_]VioletValue{} } };

    var p = VioletValue{ .pair = Pair{ .left = &a, .right = &b } };

    try testing.expect(p.pair.left == &a);
}
