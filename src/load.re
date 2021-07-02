    // <load> "load" wsnl* "(" wsnl* "\"" @s1 string_lit @s2 "\"" wsnl* "," wsnl* ( (#soid identifier #eoid "=")? "\"" #sosym string_lit #eosym "\"" wsnl* ","? wsnl*)+ wsnl* ")" => init {
    //     log_debug("LOAD");
    //     log_debug("soid: %d, eoid: %d", soid, eoid);
    //     log_debug("sosym: %d, eosym: %d", sosym, eosym);
    //     /* lexer->pos.col = lexer->tok - lexer->sol; */
    //     (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok);
    //     log_debug("string: %s", (*mtok)->s);
    //     (*mtok)->load.label = strndup(s1, (size_t)(s2 - s1));
    //     log_debug("Subexpr ct: %d", utarray_len((*mtok)->load.syms));
    //     log_debug("load.syms ptr: %p", (*mtok)->load.syms);
    //     return TK_LOAD;
    // }
