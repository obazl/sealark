#include "constants.h"

/* WARNING WARNING: these #defines must be kept in sync with syntaxis.h */
#if EXPORT_INTERFACE
#define TK_Unspecified                    136
#define TK_Unary_Expr                     135
#define TK_Target_List                    134
#define TK_Target                         133
#define TK_Stmt_List                      132
#define TK_Stmt                           131
#define TK_Small_Stmt_List                130
#define TK_Slice_Sfx                      129
#define TK_Slice_Expr                     128
#define TK_Return_Expr                    127
#define TK_Paren_Expr                     126
#define TK_Primary_Expr                   125
#define TK_Param_Star2                    124
#define TK_Param_Star                     123
#define TK_Param_Named                    122
#define TK_Param_List                     121
#define TK_Node_List                      120
#define TK_Loop_Vars                      119
#define TK_Load_Stmt                      118
#define TK_List_Expr                      117
#define TK_List_Comp                      116
#define TK_Lambda_Expr                    115
#define TK_Indent_Block                   114
#define TK_If_Stmt                        113
#define TK_If_Expr                        112
#define TK_For_Stmt                       111
#define TK_Expr_List                      110
#define TK_Expr                           109
#define TK_Dot_Sfx                        108
#define TK_Dot_Expr                       107
#define TK_Dict_Expr                      106
#define TK_Dict_Entry_List                105
#define TK_Dict_Entry                     104
#define TK_Dict_Comp                      103
#define TK_Def_Stmt                       102
#define TK_Def_Compound                   101
#define TK_Comp_Clause                    100
#define TK_Call_Sfx                       99
#define TK_Call_Expr                      98
#define TK_Build_File                     97
#define TK_Bin_Expr                       96
#define TK_Assign_Stmt                    95
#define TK_Attr_Value                     94
#define TK_Attr_Name                      93
#define TK_Attr                           92
#define TK_Arg_Star2                      91
#define TK_Arg_Star                       90
#define TK_Binding                      89
#define TK_Arg_List                       88
#define TK_ALIAS                          87
#define TK_YIELD                          86
#define TK_WITH                           85
#define TK_WHILE                          84
#define TK_VBAR_EQ                        83
#define TK_VBAR                           82
#define TK_TRY                            81
#define TK_TILDE                          80
#define TK_STRING                         79
#define TK_STAR2                          78
#define TK_STAR_EQ                        77
#define TK_STAR                           76
#define TK_SLASH                          75
#define TK_SEMI                           74
#define TK_RRANGLE_EQ                     73
#define TK_RRANGLE                        72
#define TK_RPAREN                         71
#define TK_RETURN                         70
#define TK_RBRACK                         69
#define TK_RBRACE                         68
#define TK_RANGLE                         67
#define TK_RAISE                          66
#define TK_PLUS_EQ                        65
#define TK_PLUS                           64
#define TK_PCT_EQ                         63
#define TK_PCT                            62
#define TK_PASS                           61
#define TK_OR                             60
#define TK_NOT                            59
#define TK_NONLOCAL                       58
#define TK_NEWLINE                        57
#define TK_MINUS_EQ                       56
#define TK_MINUS                          55
#define TK_LPAREN                         54
#define TK_LOAD                           53
#define TK_LLANGLE_EQ                     52
#define TK_LLANGLE                        51
#define TK_LE                             50
#define TK_LBRACK                         49
#define TK_LBRACE                         48
#define TK_LANGLE                         47
#define TK_LAMBDA                         46
#define TK_IS                             45
#define TK_IN                             44
#define TK_INT_OCT                        43
#define TK_INT_HEX                        42
#define TK_INT_DEC                        41
#define TK_INT                            40
#define TK_IMPORT                         39
#define TK_IF                             38
#define TK_ID                             37
#define TK_GLOBAL                         36
#define TK_GE                             35
#define TK_FROM                           34
#define TK_FOR                            33
#define TK_FLOAT                          32
#define TK_FLOAT_LIT                      31
#define TK_FINALLY                        30
#define TK_EXCEPT                         29
#define TK_BACKSLASH2                     28
#define TK_EQ2                            27
#define TK_EQ                             26
#define TK_ELSE                           25
#define TK_ELIF                           24
#define TK_DOT                            23
#define TK_SLASH_EQ                       22
#define TK_SLASH2_EQ                      21
#define TK_SLASH2                         20
#define TK_DEL                            19
#define TK_DEF                            18
#define TK_CONTINUE                       17
#define TK_COMMENT                        16
#define TK_COMMA                          15
#define TK_COLON                          14
#define TK_CLASS                          13
#define TK_CARET_EQ                       12
#define TK_CARET                          11
#define TK_BREAK                          10
#define TK_BLANK                           9
#define TK_BANG_EQ                         8
#define TK_BANG                            7
#define TK_ASSERT                          6
#define TK_AS                              5
#define TK_ARROW                           4
#define TK_AND                             3
#define TK_AMP_EQ                          2
#define TK_AMP                             1
#endif

/* const char *token_name[256][2] = */
/*     { */
/*      [TK_ALIAS] = { "TK_ALIAS", "" }, */
/*      [TK_AMP] = { "TK_AMP", "&" }, */
/*      [TK_AMP_EQ] = { "TK_AMP_EQ", "&=" }, */
/*      [TK_AND] = { "TK_AND", "and" }, */
/*      [TK_ARROW] = {"TK_ARROW", "->"}, */
/*      [TK_AS] = { "TK_AS", "as" }, */
/*      [TK_ASSERT] = { "TK_ASSERT", "assert" }, */
/*      [TK_BANG] = { "TK_BANG", "!" }, */
/*      [TK_BANG_EQ] = { "TK_BANG_EQ", "!=" }, */
/*      [TK_BREAK] = { "TK_BREAK", "break" }, */
/*      [TK_BLANK] = { "TK_BLANK", "" }, */
/*      [TK_CARET] = { "TK_CARET", "^" }, */
/*      [TK_CARET_EQ] = { "TK_CARET_EQ", "^=" }, */
/*      [TK_CLASS] = { "TK_CLASS", "class" }, */
/*      [TK_COLON] = { "TK_COLON", ":" }, */
/*      [TK_ICOLON] = { "TK_ICOLON", ":" }, */
/*      [TK_COMMA] = { "TK_COMMA", "," }, */
/*      [TK_COMMENT] = { "TK_COMMENT", "" }, */
/*      [TK_CONTINUE] = { "TK_CONTINUE", "continue" }, */
/*      [TK_DEF] = { "TK_DEF", "def" }, */
/*      [TK_DEL] = { "TK_DEL", "del" }, */
/*      [TK_SLASH2] = { "TK_SLASH2", "//" }, */
/*      [TK_SLASH2_EQ] = { "TK_SLASH2_EQ", "//=" }, */
/*      [TK_DIV_EQ] = { "TK_DIV_EQ", "/=" }, */
/*      [TK_DOT] = { "TK_DOT", "." }, */
/*      [TK_DQ] = { "TK_DQ", "\"" }, */
/*      [TK_ELIF] = { "TK_ELIF", "elif" }, */
/*      [TK_ELSE] = { "TK_ELSE", "else" }, */
/*      [TK_EQ] = { "TK_EQ", "=" }, */
/*      [TK_EQ2] = { "TK_EQ2", "==" }, */
/*      [TK_ESC_BACKSLASH] = { "TK_ESC_BACKSLASH", "\\" }, */
/*      [TK_EXCEPT] = { "TK_EXCEPT", "except" }, */
/*      [TK_FINALLY] = { "TK_FINALLY", "finally" }, */
/*      [TK_FLOAT] = { "TK_FLOAT", "float" }, */
/*      [TK_FLOAT_LIT] = { "TK_FLOAT_LIT", "" }, */
/*      [TK_FOR] = { "TK_FOR", "for" }, */
/*      [TK_FROM] = { "TK_FROM", "from" }, */
/*      [TK_GE] = {"TK_GE", ">="}, */
/*      [TK_GLOBAL] = { "TK_GLOBAL", "global" }, */
/*      [TK_ID] = { "TK_ID", "" }, */
/*      [TK_IF] = { "TK_IF", "if" }, */
/*      [TK_IMPORT] = { "TK_IMPORT", "import" }, */
/*      [TK_INT_DEC] = { "TK_INT_DEC", "" }, */
/*      [TK_INT_HEX] = { "TK_INT_HEX", "" }, */
/*      [TK_INT_OCT] = { "TK_INT_OCT", "" }, */
/*      [TK_INT] = { "TK_INT", "" }, */
/*      [TK_IN] = { "TK_IN", "in" }, */
/*      [TK_IS] = { "TK_IS", "is" }, */
/*      [TK_LAMBDA] = { "TK_LAMBDA", "lambda" }, */
/*      [TK_LANGLE] = { "TK_LANGLE", "<" }, */
/*      [TK_LBRACE] = { "TK_LBRACE", "{" }, */
/*      [TK_LBRACK] = { "TK_LBRACK", "[" }, */
/*      [TK_LE] = {"TK_LE", "<="}, */
/*      [TK_LLANGLE] = { "TK_LLANGLE", "<<" }, */
/*      [TK_LLANGLE_EQ] = { "TK_LLANGLE_EQ", "<<=" }, */
/*      [TK_LOAD] = { "TK_LOAD", "load" }, */
/*      [TK_LPAREN] = { "TK_LPAREN", "(" }, */
/*      [TK_MINUS] = { "TK_MINUS", "-" }, */
/*      [TK_MINUS_EQ] = { "TK_MINUS_EQ", "-=" }, */
/*      [TK_NONLOCAL] = { "TK_NONLOCAL", "nonlocal" }, */
/*      [TK_NOT] = { "TK_NOT", "not" }, */
/*      [TK_OR] = { "TK_OR", "or" }, */
/*      [TK_PASS] = { "TK_PASS", "pass" }, */
/*      [TK_PCT] = { "TK_PCT", "%" }, */
/*      [TK_PCT_EQ] = { "TK_PCT_EQ", "%=" }, */
/*      [TK_PLUS] = { "TK_PLUS", "+" }, */
/*      [TK_PLUS_EQ] = { "TK_PLUS_EQ", "+=" }, */
/*      [TK_RAISE] = { "TK_RAISE", "raise" }, */
/*      [TK_RANGLE] = { "TK_RANGLE", ">" }, */
/*      [TK_RBRACE] = { "TK_RBRACE", "}" }, */
/*      [TK_RBRACK] = { "TK_RBRACK", "]" }, */
/*      [TK_RETURN] = { "TK_RETURN", "return" }, */
/*      [TK_RPAREN] = { "TK_RPAREN", ")" }, */
/*      [TK_RRANGLE] = { "TK_RRANGLE", ">>" }, */
/*      [TK_RRANGLE_EQ] = { "TK_RRANGLE_EQ", ">>=" }, */
/*      [TK_SEMI] = { "TK_SEMI", ";" }, */
/*      [TK_SLASH] = { "TK_SLASH", "/" }, */
/*      [TK_SQ] = { "TK_SQ", "'" }, */
/*      [TK_STAR] = { "TK_STAR", "*" }, */
/*      [TK_STAR2] = { "TK_STAR2", "**" }, */
/*      [TK_STAR_EQ] = { "TK_STAR_EQ", "*=" }, */
/*      [TK_STRING] = { "TK_STRING", ""}, */
/*      [TK_BSTRING] = { "TK_BSTRING", ""},    /\* byte string *\/ */
/*      [TK_BRSTRING] = { "TK_BRSTRING", ""},    /\* raw byte string *\/ */
/*      [TK_RSTRING] = { "TK_RSTRING", ""}, */
/*      [TK_RBSTRING] = { "TK_RBSTRING", ""},    /\* raw byte string *\/ */
/*      [TK_MLSTRING] = { "TK_MLSTRING", ""},    /\* multi-line string *\/ */
/*      [TK_MLBSTRING] = { "TK_MLBSTRING", ""}, */
/*      [TK_MLBRSTRING] = { "TK_MLBRSTRING", ""}, */
/*      [TK_MLRSTRING] = { "TK_MLRSTRING", ""}, */
/*      [TK_MLRBSTRING] = { "TK_MLRBSTRING", ""}, */
/*      [TK_TILDE] = { "TK_TILDE", "~" }, */
/*      [TK_TRY] = { "TK_TRY", "try" }, */
/*      [TK_VBAR] = { "TK_VBAR", "|" }, */
/*      [TK_VBAR_EQ] = { "TK_VBAR_EQ", "|=" }, */
/*      [TK_WHILE] = { "TK_WHILE", "while" }, */
/*      [TK_WITH] = { "TK_WITH", "with" }, */
/*      [TK_YIELD] = { "TK_YIELD", "yield" }, */
/*      [TK_NEWLINE] = { "TK_NEWLINE", "" }, */

/*      /\* non-terminals *\/ */
/*      [TK_Arg_List] = { "TK_Arg_List", "" }, */
/*      [TK_Binding] = { "TK_Binding", "" }, */
/*      [TK_Arg_Star] = { "TK_Arg_Star", "" }, */
/*      [TK_Arg_Star2] = { "TK_Arg_Star2", "" }, */
/*      [TK_Assign_Stmt] = { "TK_Assign_Stmt", "" }, */
/*      [TK_Attr] = { "TK_Attr", "" }, */
/*      [TK_Attr_Name] = { "TK_Attr_Name", "" }, */
/*      [TK_Attr_Value] = { "TK_Attr_Value", "" }, */
/*      [TK_Bin_Expr] = { "TK_Bin_Expr", "" }, */
/*      [TK_Build_File] = { "TK_Build_File", "" }, */
/*      [TK_Build_Target] = { "TK_Build_Target", "" }, */
/*      [TK_Call_Expr] = { "TK_Call_Expr", "" }, */
/*      [TK_Call_Sfx] = { "TK_Call_Sfx", "" }, */
/*      [TK_Comp_Clause] = { "TK_Comp_Clause", "" }, */
/*      [TK_Def_Stmt] = { "TK_Def_Stmt", "" }, */
/*      [TK_Dict_Comp] = { "TK_Dict_Comp", "" }, */
/*      [TK_Dict_Entry] = { "TK_Dict_Entry", "" }, */
/*      [TK_Dict_Entry_List] = { "TK_Dict_Entry_List", "" }, */
/*      [TK_Dict_Expr] = { "TK_Dict_Expr", "" }, */
/*      [TK_Dot_Expr] = { "TK_Dot_Expr", "" }, */
/*      [TK_Dot_Sfx] = { "TK_Dot_Sfx", "" }, */
/*      [TK_Expr] = { "TK_Expr", "" }, */
/*      [TK_Expr_List] = { "TK_Expr_List", "" }, */
/*      [TK_Expr] = { "TK_Expr", "" }, */
/*      [TK_For_Stmt] = { "TK_For_Stmt", "" }, */
/*      [TK_If_Expr] = { "TK_If_Expr", "" }, */
/*      [TK_If_Stmt] = { "TK_If_Stmt", "" }, */
/*      [TK_Indent_Block] = { "TK_Indent_Block", "" }, */
/*      [TK_Lambda_Expr] = { "TK_Lambda_Expr", "" }, */
/*      [TK_List_Comp] = { "TK_List_Comp", "" }, */
/*      [TK_List_Expr] = { "TK_List_Expr", "" }, */
/*      [TK_Load_Stmt] = { "TK_Load_Stmt", "" }, */
/*      [TK_Loop_Vars] = { "TK_Loop_Vars", "" }, */
/*      [TK_Param_List] = { "TK_Param_List", "" }, */
/*      [TK_Param_Named] = { "TK_Param_Named", "" }, */
/*      [TK_Param_Star] = { "TK_Param_Star", "" }, */
/*      [TK_Param_Star2] = { "TK_Param_Star2", "" }, */
/*      [TK_Paren_Expr] = { "TK_Paren_Expr", "" }, */
/*      [TK_Primary_Expr] = { "TK_Primary_Expr", "" }, */
/*      [TK_Return_Expr] = { "TK_Return_Expr", "" }, */
/*      [TK_Slice_Sfx] = { "TK_Slice_Sfx", "" }, */
/*      [TK_Slice_Expr] = { "TK_Slice_Expr", "" }, */
/*      [TK_SmallStmt_List] = { "TK_SmallStmt_List", "" }, */
/*      [TK_Stmt] = { "TK_Stmt", "" }, */
/*      [TK_Stmt_List] = { "TK_Stmt_List", "" }, */
/*      [TK_Unary_Expr] = { "TK_Unary_Expr", "" }, */
/*      [TK_Node_List] = { "TK_Node_List", "" }, */

/*      NULL */
/*     }; */
