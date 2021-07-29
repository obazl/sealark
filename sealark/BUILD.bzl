def _gensyntax_impl(ctx):
    out_c = ctx.actions.declare_file("syntaxis.c")
    out_out = ctx.actions.declare_file("syntaxis.out")

    exe = ctx.file._tool.path

    print(ctx.attr.defines)

    if (len(ctx.attr.defines) > 0):
        defs = "-D" + " -D".join(ctx.attr.defines)
    else:
        defs = ""

    cmd = "{lemon} -m {yy} {defines} -T{template} -d{outdir}".format(
        lemon=exe, yy=ctx.file.yy.path,
        defines=defs,
        template=ctx.file.template.path,
        outdir=out_c.dirname
    )
    ctx.actions.run_shell(
        inputs = [ctx.file.yy, ctx.file.template],
        outputs = ctx.outputs.outs,
        tools = [ctx.file._tool],
        command = cmd
    )

    return [DefaultInfo(files = depset(ctx.outputs.outs))]

gensyntax = rule(
    implementation = _gensyntax_impl,
    attrs = {
        "yy": attr.label(
            allow_single_file = True,
            default = "syntaxis.y"
        ),
        "outs": attr.output_list( ),
        "defines": attr.string_list(
        ),
        "template": attr.label(
            allow_single_file =  True,
            default = "//vendored/lemon:lempar.c"
        ),
        "_tool": attr.label(
            allow_single_file = True,
            default = "//vendored/lemon",
            executable = True,
            cfg = "host"
        )
    }
)
