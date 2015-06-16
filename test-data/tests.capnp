@0x8375117c268d7f89;

struct OneBool {
  bool @0 :Bool;
}

struct ManyBool {
  field0 @0 :Bool;
  field1 @1 :Bool;
  field2 @2 :Bool;
  field3 @3 :Bool;
  field4 @4 :Bool;
  field5 @5 :Bool;
  field6 @6 :Bool;
  field7 @7 :Bool;
  field8 @8 :Bool;
  field9 @9 :Bool;
  field10 @10 :Bool;
  field11 @11 :Bool;
  field12 @12 :Bool;
  field13 @13 :Bool;
  field14 @14 :Bool;
  field15 @15 :Bool;
  field16 @16 :Bool;
  field17 @17 :Bool;
  field18 @18 :Bool;
  field19 @19 :Bool;
  field20 @20 :Bool;
  field21 @21 :Bool;
  field22 @22 :Bool;
  field23 @23 :Bool;
  field24 @24 :Bool;
  field25 @25 :Bool;
  field26 @26 :Bool;
  field27 @27 :Bool;
  field28 @28 :Bool;
  field29 @29 :Bool;
  field30 @30 :Bool;
  field31 @31 :Bool;
  field32 @32 :Bool;
  field33 @33 :Bool;
  field34 @34 :Bool;
  field35 @35 :Bool;
  field36 @36 :Bool;
  field37 @37 :Bool;
  field38 @38 :Bool;
  field39 @39 :Bool;
  field40 @40 :Bool;
  field41 @41 :Bool;
  field42 @42 :Bool;
  field43 @43 :Bool;
  field44 @44 :Bool;
  field45 @45 :Bool;
  field46 @46 :Bool;
  field47 @47 :Bool;
  field48 @48 :Bool;
  field49 @49 :Bool;
  field50 @50 :Bool;
  field51 @51 :Bool;
  field52 @52 :Bool;
  field53 @53 :Bool;
  field54 @54 :Bool;
  field55 @55 :Bool;
  field56 @56 :Bool;
  field57 @57 :Bool;
  field58 @58 :Bool;
  field59 @59 :Bool;
  field60 @60 :Bool;
  field61 @61 :Bool;
  field62 @62 :Bool;
  field63 @63 :Bool;
}

struct ManyUInt8 {
  field0 @0 :UInt8;
  field1 @1 :UInt8;
  field2 @2 :UInt8;
  field3 @3 :UInt8;
  field4 @4 :UInt8;
  field5 @5 :UInt8;
  field6 @6 :UInt8;
  field7 @7 :UInt8;
}

struct OneUInt32 {
  field0 @0 :UInt32;
}

struct ThreeStructs {
  field0 @0 :OneUInt32;
  field1 @1 :OneUInt32;
  field2 @2 :OneUInt32;
}

struct ThreeLists {
  field0 @0 :List(UInt32);
  field1 @1 :List(UInt32);
  field2 @2 :List(UInt32);
}

struct ListOfUInt32 {
  field0 @0 :List(UInt32);
}

struct ListOfUInt64 {
  field0 @0 :List(UInt64);
}

struct ListOfBool {
  field0 @0 :List(Bool);
}

struct ListOfOneUInt32 {
  field0 @0 :List(OneUInt32);
}

struct OneText {
  field0 @0 :Text;
}

struct OneData {
  field0 @0 :Data;
}

struct ListOfListOfUInt32 {
  field0 @0 :List(List(UInt32));
}

struct Mixed {
  field0 @0 :UInt32;
  field1 @1 :OneUInt32;
  field3 @2 :List(UInt32);
}
