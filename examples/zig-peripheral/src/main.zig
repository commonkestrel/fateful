const std = @import("std");

var STATE: u8 = 0;

export fn init(n: u8) void {
    _ = n;
    std.debug.print("Zig init\n", .{});
}

export fn read(n: u8) u8 {
    _ = n;
    std.debug.print("Zig read\n", .{});
    return STATE;
}

export fn write(n: u8, data: u8) void {
    _ = n;
    std.debug.print("Zig write\n", .{});
    STATE = data;  
}

export fn name() [*:0] const u8 {
    return "Zig Example";
}
