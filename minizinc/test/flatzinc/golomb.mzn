predicate all_different_int(array [int] of var int: x);
array [1..2] of int: X_INTRODUCED_9 = [1,-1];
var 1..81: X_INTRODUCED_1;
var 0..81: X_INTRODUCED_2;
var 0..81: X_INTRODUCED_3;
var 0..81: X_INTRODUCED_4;
var 0..81: X_INTRODUCED_5;
var 0..81: X_INTRODUCED_6;
var 0..81: X_INTRODUCED_7;
var 0..81: X_INTRODUCED_8;
var 0..81: X_INTRODUCED_18 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_19 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_20 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_21 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_22 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_23 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_24 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_25 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_26 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_27 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_28 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_29 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_30 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_31 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_32 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_33 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_34 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_35 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_36 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_37 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_38 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_39 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_40 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_41 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_42 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_43 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_44 ::var_is_introduced :: is_defined_var;
var 0..81: X_INTRODUCED_45 ::var_is_introduced :: is_defined_var;
array [1..9] of var int: mark:: output_array([1..9]) = [0,X_INTRODUCED_1,X_INTRODUCED_2,X_INTRODUCED_3,X_INTRODUCED_4,X_INTRODUCED_5,X_INTRODUCED_6,X_INTRODUCED_7,X_INTRODUCED_8];
array [1..36] of var int: differences = [X_INTRODUCED_1,X_INTRODUCED_2,X_INTRODUCED_3,X_INTRODUCED_4,X_INTRODUCED_5,X_INTRODUCED_6,X_INTRODUCED_7,X_INTRODUCED_8,X_INTRODUCED_18,X_INTRODUCED_19,X_INTRODUCED_20,X_INTRODUCED_21,X_INTRODUCED_22,X_INTRODUCED_23,X_INTRODUCED_24,X_INTRODUCED_25,X_INTRODUCED_26,X_INTRODUCED_27,X_INTRODUCED_28,X_INTRODUCED_29,X_INTRODUCED_30,X_INTRODUCED_31,X_INTRODUCED_32,X_INTRODUCED_33,X_INTRODUCED_34,X_INTRODUCED_35,X_INTRODUCED_36,X_INTRODUCED_37,X_INTRODUCED_38,X_INTRODUCED_39,X_INTRODUCED_40,X_INTRODUCED_41,X_INTRODUCED_42,X_INTRODUCED_43,X_INTRODUCED_44,X_INTRODUCED_45];
constraint int_lin_le(X_INTRODUCED_9,[X_INTRODUCED_1,X_INTRODUCED_2],-1);
constraint int_lin_le(X_INTRODUCED_9,[X_INTRODUCED_2,X_INTRODUCED_3],-1);
constraint int_lin_le(X_INTRODUCED_9,[X_INTRODUCED_3,X_INTRODUCED_4],-1);
constraint int_lin_le(X_INTRODUCED_9,[X_INTRODUCED_4,X_INTRODUCED_5],-1);
constraint int_lin_le(X_INTRODUCED_9,[X_INTRODUCED_5,X_INTRODUCED_6],-1);
constraint int_lin_le(X_INTRODUCED_9,[X_INTRODUCED_6,X_INTRODUCED_7],-1);
constraint int_lin_le(X_INTRODUCED_9,[X_INTRODUCED_7,X_INTRODUCED_8],-1);
constraint all_different_int(differences);
constraint int_lin_le([1,-1,1],[X_INTRODUCED_1,X_INTRODUCED_8,X_INTRODUCED_7],-1);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_2,X_INTRODUCED_1,X_INTRODUCED_18],0):: defines_var(X_INTRODUCED_18);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_3,X_INTRODUCED_1,X_INTRODUCED_19],0):: defines_var(X_INTRODUCED_19);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_4,X_INTRODUCED_1,X_INTRODUCED_20],0):: defines_var(X_INTRODUCED_20);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_5,X_INTRODUCED_1,X_INTRODUCED_21],0):: defines_var(X_INTRODUCED_21);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_6,X_INTRODUCED_1,X_INTRODUCED_22],0):: defines_var(X_INTRODUCED_22);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_7,X_INTRODUCED_1,X_INTRODUCED_23],0):: defines_var(X_INTRODUCED_23);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_8,X_INTRODUCED_1,X_INTRODUCED_24],0):: defines_var(X_INTRODUCED_24);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_3,X_INTRODUCED_2,X_INTRODUCED_25],0):: defines_var(X_INTRODUCED_25);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_4,X_INTRODUCED_2,X_INTRODUCED_26],0):: defines_var(X_INTRODUCED_26);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_5,X_INTRODUCED_2,X_INTRODUCED_27],0):: defines_var(X_INTRODUCED_27);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_6,X_INTRODUCED_2,X_INTRODUCED_28],0):: defines_var(X_INTRODUCED_28);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_7,X_INTRODUCED_2,X_INTRODUCED_29],0):: defines_var(X_INTRODUCED_29);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_8,X_INTRODUCED_2,X_INTRODUCED_30],0):: defines_var(X_INTRODUCED_30);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_4,X_INTRODUCED_3,X_INTRODUCED_31],0):: defines_var(X_INTRODUCED_31);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_5,X_INTRODUCED_3,X_INTRODUCED_32],0):: defines_var(X_INTRODUCED_32);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_6,X_INTRODUCED_3,X_INTRODUCED_33],0):: defines_var(X_INTRODUCED_33);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_7,X_INTRODUCED_3,X_INTRODUCED_34],0):: defines_var(X_INTRODUCED_34);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_8,X_INTRODUCED_3,X_INTRODUCED_35],0):: defines_var(X_INTRODUCED_35);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_5,X_INTRODUCED_4,X_INTRODUCED_36],0):: defines_var(X_INTRODUCED_36);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_6,X_INTRODUCED_4,X_INTRODUCED_37],0):: defines_var(X_INTRODUCED_37);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_7,X_INTRODUCED_4,X_INTRODUCED_38],0):: defines_var(X_INTRODUCED_38);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_8,X_INTRODUCED_4,X_INTRODUCED_39],0):: defines_var(X_INTRODUCED_39);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_6,X_INTRODUCED_5,X_INTRODUCED_40],0):: defines_var(X_INTRODUCED_40);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_7,X_INTRODUCED_5,X_INTRODUCED_41],0):: defines_var(X_INTRODUCED_41);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_8,X_INTRODUCED_5,X_INTRODUCED_42],0):: defines_var(X_INTRODUCED_42);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_7,X_INTRODUCED_6,X_INTRODUCED_43],0):: defines_var(X_INTRODUCED_43);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_8,X_INTRODUCED_6,X_INTRODUCED_44],0):: defines_var(X_INTRODUCED_44);
constraint int_lin_eq([1,-1,-1],[X_INTRODUCED_8,X_INTRODUCED_7,X_INTRODUCED_45],0):: defines_var(X_INTRODUCED_45);
solve :: int_search(mark,input_order,indomain,complete) minimize X_INTRODUCED_8;
