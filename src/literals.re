    /* LITERALS: integer, floating point, string, byte  */

    raw_sqstring_lit = [^']+;
    <rawsqstr> @s1 raw_sqstring_lit @s2 "'" => init {
            (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok);
            log_debug("<rawsqstr>: %s", (*mtok)->s);
            /* lexer->clean_line = false; */
            /* lexer->pos.col = lexer->tok - lexer->sol; */
            (*mtok)->line = lexer->pos.line;
            (*mtok)->col  = lexer->tok - lexer->sol;
            /* (*mtok)->s = strndup(s1, (size_t)(s2 - s1)); */
            return TK_RAWSTRING;
      }
    <init> "r'" :=> rawsqstr

    raw_dqstring_lit = [^"]+;
    <rawdqstr> @s1 raw_dqstring_lit @s2 "\"" => init {
            (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok);
            log_debug("<rawdqstr>: %s", (*mtok)->s);
            /* lexer->clean_line = false; */
            /* lexer->pos.col = lexer->tok - lexer->sol; */
            (*mtok)->line = lexer->pos.line;
            (*mtok)->col  = lexer->tok - lexer->sol;
            /* (*mtok)->s = strndup(s1, (size_t)(s2 - s1)); */
            return TK_RAWSTRING;
      }
    <init> "r\"" :=> rawdqstr

    sqstring_lit = [^'\n]+;
    <sqstr> @s1 sqstring_lit @s2 "'" => init {
            log_debug("<sqstr>, mode: %d", lexer->mode);
            (*mtok)->line = lexer->pos.line;
            (*mtok)->col  = lexer->tok - lexer->sol;
            (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok);
            return TK_STRING;
      }
    <init> "\'" :=> sqstr

    dqstring_lit = [^"\n]+;
    <dqstr> @s1 dqstring_lit @s2 "\"" => init {
            (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok);
            log_debug("<dqstr>: %s", (*mtok)->s);
            /* lexer->clean_line = false; */
            /* lexer->pos.col = lexer->tok - lexer->sol; */
            (*mtok)->line = lexer->pos.line;
            (*mtok)->col  = lexer->tok - lexer->sol;
            /* (*mtok)->s = strndup(s1, (size_t)(s2 - s1)); */
            return TK_STRING;
      }
    <init> "\"" :=> dqstr

    // MULTI-LINE (TRIPLE-QUOTED) STRINGS
    dq3string_lit = [^"]+;
    <dq3str> @s1 dq3string_lit @s2 "\"\"\"" => init {
            (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok);
            log_debug("<dq3str>: %s", (*mtok)->s);
            /* lexer->clean_line = false; */
            /* lexer->pos.col = lexer->tok - lexer->sol; */
            (*mtok)->line = lexer->pos.line;
            (*mtok)->col  = lexer->tok - lexer->sol;
            /* (*mtok)->s = strndup(s1, (size_t)(s2 - s1)); */
            return TK_MLSTRING;
      }
    <init> "\"\"\"" :=> dq3str

    // BYTESTRINGS
    /* single-quoted, e.g. b'hello' */
    sqbytestr_lit = [^']+;
    <sqbytes> @s1 sqbytestr_lit @s2 "'" => init {
        (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok);
        return TK_BYTESTR;
    }
    <init> "b'" :=> sqbytes

    /* double-quoted, e.g. b"hello" */
    dqbytestr_lit = [^"]+;
    <dqbytes> @s1 dqbytestr_lit @s2 "\"" => init {
        (*mtok)->s = strndup(lexer->tok, lexer->cursor - lexer->tok);
        return TK_BYTESTR;
    }
    <init> "b\"" :=> dqbytes

