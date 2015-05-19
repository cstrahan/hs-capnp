#!/usr/bin/env python

import os, inspect
import capnp
import tests_capnp

test_data_dir = os.path.dirname(os.path.abspath(__file__))

################################################################################
# Sanity checks for handwritten messages

def read(schema, name):
    p = os.path.join(test_data_dir, name)
    f = open(p, 'rb')
    return schema.read(f)

struct = read(tests_capnp.OneUInt32, "far_one_uint32")
assert struct.field0 == 0x11223344, "far_one_uint32 sanity-check failed"

struct = read(tests_capnp.OneUInt32, "double_far_one_uint32")
assert struct.field0 == 0x11223344, "double_far_one_uint32 sanity-check failed"

################################################################################
# Create test cases

def write(message, name):
    p = os.path.join(test_data_dir, name)
    f = open(p, 'w+b')
    message.write(f)

struct = tests_capnp.OneBool.new_message()
struct.bool = False
write(struct, "one_bool_false")

struct = tests_capnp.OneBool.new_message()
struct.bool = True
write(struct, "one_bool_true")

struct = tests_capnp.OneUInt32.new_message()
struct.field0 = 0x11223344
write(struct, "one_uint32")

struct = tests_capnp.ManyBool.new_message()
for n in range(0, 64):
    setattr(struct, "field"+str(n), True)
write(struct, "many_bool")

struct = tests_capnp.ManyUInt8.new_message()
for n in range(0, 8):
    setattr(struct, "field"+str(n), n+1)
write(struct, "many_uint8")

struct = tests_capnp.ThreeStructs.new_message()
struct.field0.field0 = 0x11223344
struct.field1.field0 = 0x55667788
struct.field2.field0 = 0x99AABBCC
write(struct, "three_structs")

struct = tests_capnp.ListOfUInt32.new_message()
struct.field0 = [0x11223344, 0x55667788, 0x99AABBCC]
write(struct, "list_of_uint32")

struct = tests_capnp.ListOfUInt64.new_message()
struct.field0 = [0x1020304050607080, 0x1121314151617181, 0x1222324252627282]
write(struct, "list_of_uint64")

struct = tests_capnp.ListOfBool.new_message()
struct.field0 = [True, True, False, True]
write(struct, "list_of_bool")

struct = tests_capnp.ListOfOneUInt32.new_message()
l = struct.init("field0", 3)
l[0].field0 = 0x11223344
l[1].field0 = 0x55667788
l[2].field0 = 0x99AABBCC
write(struct, "list_of_one_uint32")

struct = tests_capnp.OneText.new_message()
struct.field0 = "This is some text."
write(struct, "one_text")

struct = tests_capnp.OneData.new_message()
struct.field0 = "This is some data."
write(struct, "one_data")
