const std = @import("std");

var STATE: u8 = 0;

export fn init() void {
    std.debug.print("Zig init\n", .{});
}

export fn read() u8 {
    std.debug.print("Zig read\n", .{});
    return STATE;
}

export fn write(data: u8) void {
    std.debug.print("Zig write\n", .{});
    STATE = data;  
}

export fn name() [*:0] const u8 {
    return "Zig Example";
}
