#include "s7.h"

#include "sunlark_errors.h"

s7_pointer handle_errno(s7_scheme *s7, int errorno, s7_pointer path_args)
{
    switch(errorno) {
    case ENOT_IMPLEMENTED:
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                                                      "Not yet implemented: ~A"),
                                path_args)));
        break;
    case ETOO_MANY_ARGS:
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                                                      "Too many args: ~A"),
                                path_args)));
        break;
    case ETOO_MANY_ARGS_BINDINGS:
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                                "Too many args: ~A - :bindings must be last arg."),
                                path_args)));
        break;
    case EMISSING_ARG_BINDING:
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                        "Invalid: ~A - missing selector after :binding"),
                                path_args)));
        break;
    case EMISSING_ARG_SELECTOR:
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                                                      "~A: missing selector after :arg"),
                                path_args)));
        break;
    case ENOT_FOUND:
        return(s7_error(s7, s7_make_symbol(s7, "not_found"),
                        s7_list(s7, 2, s7_make_string(s7,
                                                      "Not found: ~A"),
                                (path_args))));
        break;
    case ENOT_FOUND_BINDING:
        return(s7_error(s7, s7_make_symbol(s7, "not_found"),
                        s7_list(s7, 2, s7_make_string(s7,
                           "Binding not found for: ~A"),
                                (path_args))));
        break;
    case EINDEX_OUT_OF_BOUNDS:
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                           "Index out of bounds: ~A"),
                                (path_args))));
        break;
    case EINVALID_UPDATE:
        return(s7_error(s7, s7_make_symbol(s7, "invalid_update"),
                        s7_list(s7, 2, s7_make_string(s7,
                 "Invalid update value: ~A"),
                                (path_args))));
        break;
    case EINVALID_ARG_LOAD:
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                                                      "Invalid arg ~A following :load"),
                                path_args)));
        break;
    case ENULL_BINDING_VAL:
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                        "Cannot set binding value to null: ~A"),
                                path_args)));
        break;
    default:
        return(s7_error(s7, s7_make_symbol(s7, "invalid_argument"),
                        s7_list(s7, 2, s7_make_string(s7,
                                                      "Error: errno ~D on path_args: ~A"),
                                s7_make_integer(s7,errorno),
                                path_args)));
    }
}
