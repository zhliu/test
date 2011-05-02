open Pretty
open Cil
open Cil_types

module FC_file = File
open Cil_datatype

let file_name="/home/lzh/document/cil-1.3.7/test/small1/func.c";;

Frontc.parse file_name;;
!Db.Value.compute ();
Mergecil.merge file_name file;;