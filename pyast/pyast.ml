include Pyparse

module V2_5 = V2_5

module V2_6 = V2_6

module V2_7 = V2_7

module V3_0 = V3_0

module V3_0_1 = V3_0_1

module V3_1 = V3_1

module V3_2 = V3_2

module V3_3_0 = V3_3_0

module V3_3_1 = V3_3_1

module V3_4_0 = V3_4_0

module V3_5_0 = V3_5_0

module V3_6_0 = V3_6_0

module V3_7_0 = V3_7_0

module V3_8_0 = V3_8_0

module V3_9_0 = V3_9_0

module Latest = V3_9_0

let versions : (module S) list = [
  (module V2_5);
  (module V2_6);
  (module V2_7);
  (module V3_0);
  (module V3_0_1);
  (module V3_1);
  (module V3_2);
  (module V3_3_0);
  (module V3_3_1);
  (module V3_4_0);
  (module V3_5_0);
  (module V3_6_0);
  (module V3_7_0);
  (module V3_8_0);
  (module V3_9_0)
]

let () =
  get_version_ref := fun (v : Version.t) : (module S) ->
  if v < { major = 2; minor = 5; subminor = 0 } then
    raise (UnsupportedVersion v)
  else if v < { major = 2; minor = 6; subminor = 0 } then
    (module V2_5)
  else if v < { major = 2; minor = 7; subminor = 0 } then
    (module V2_6)
  else if v < { major = 3; minor = 0; subminor = 0 } then
    (module V2_7)
  else if v < { major = 3; minor = 0; subminor = 1 } then
    (module V3_0)
  else if v < { major = 3; minor = 1; subminor = 0 } then
    (module V3_0_1)
  else if v < { major = 3; minor = 2; subminor = 0 } then
    (module V3_1)
  else if v < { major = 3; minor = 3; subminor = 0 } then
    (module V3_2)
  else if v < { major = 3; minor = 3; subminor = 1 } then
    (module V3_3_0)
  else if v < { major = 3; minor = 4; subminor = 0 } then
    (module V3_3_1)
  else if v < { major = 3; minor = 5; subminor = 0 } then
    (module V3_4_0)
  else if v < { major = 3; minor = 6; subminor = 0 } then
    (module V3_5_0)
  else if v < { major = 3; minor = 7; subminor = 0 } then
    (module V3_6_0)
  else if v < { major = 3; minor = 8; subminor = 0 } then
    (module V3_7_0)
  else if v < { major = 3; minor = 9; subminor = 0 } then
    (module V3_8_0)
  else if v < { major = 3; minor = 10; subminor = 0 } then
    (module V3_9_0)
  else
    raise (UnsupportedVersion v)
