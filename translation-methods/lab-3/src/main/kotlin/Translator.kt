import org.antlr.v4.runtime.Token
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.TerminalNode
import java.lang.StringBuilder

class Translator: GrammarBaseVisitor<Unit>() {
    private val builder = StringBuilder()
    private var level = 0
    private val offset: String
        get() {
            return TAB.repeat(level)
        }
    val result: String
        get() {
            return builder.toString()
        }


    override fun visit(tree: ParseTree?) {
        tree?.accept(this)
    }

    private var currentType: String = ES
    private var firstState: Boolean = false

    override fun visitTerminal(node: TerminalNode?) {
        node ?: return

        if (node.symbol.type != Token.EOF) {
            builder.append(node.text)
        }
    }

    override fun visitFile(ctx: GrammarParser.FileContext?) {
        visit(ctx?.package_())

        val imports = ctx?.imports()

        if (imports != null && imports.size != 0) {
            for (import in imports) {
                visit(import)
            }

            builder.append(NL)
        }

        visit(ctx?.p_class())

        val classes = ctx?.class_()

        if (classes != null && classes.size != 0) {
            for (cl in classes) {
                builder.append(NL)
                visit(cl)
            }
        }
    }

    override fun visitPackage_(ctx: GrammarParser.Package_Context?) {
        ctx ?: return

        builder.append("package").append(WS)
        builder.append(ctx.LONGNAME().text)
        builder.append(";").append(NL).append(NL)
    }

    override fun visitImports(ctx: GrammarParser.ImportsContext?) {
        ctx ?: return

        builder.append("import").append(WS)
        builder.append(ctx.class_name().text)
        builder.append(";").append(NL)
    }

    override fun visitP_class(ctx: GrammarParser.P_classContext?) {
        ctx ?: return

        builder.append("public").append(WS)
        visit(ctx.class_())
    }

    override fun visitClass_(ctx: GrammarParser.Class_Context?) {
        ctx ?: return

        builder
            .append("class")
            .append(WS)
            .append(ctx.SHORTNAME().text)
            .append(WS)

        val extends = ctx.extend()

        if (extends != null) {
            builder.append("extends").append(WS)
            builder.append(extends.class_name().text).append(WS)
        }

        val implements = ctx.implementings()

        if (implements != null) {
            builder.append("implements").append(WS)
            builder.append(implements.class_name().text)

            for (impl in implements.implement_interfaces()) {
                visit(impl)
            }

            builder.append(WS)
        }

        builder.append("{")
        level++

        for (entity in ctx.class_body()) {
            builder.append(NL)
            visit(entity)
        }

        builder.append(NL).append("}").append(NL)

        level--
    }

    override fun visitImplement_interfaces(ctx: GrammarParser.Implement_interfacesContext?) {
        ctx ?: return

        builder.append(",").append(WS)
        builder.append(ctx.class_name().text)
    }

    override fun visitClass_body(ctx: GrammarParser.Class_bodyContext?) {
        visit(ctx?.field())
        visit(ctx?.method())
    }

    override fun visitField_decorators(ctx: GrammarParser.Field_decoratorsContext?) {
        ctx ?: return

        visit(ctx.access_modifiers())
        visit(ctx.modifications())
    }

    override fun visitField(ctx: GrammarParser.FieldContext?) {
        ctx ?: return

        builder.append(offset)

        visit(ctx.field_decorators())
        visit(ctx.type())
        builder.append(WS)
        visit(ctx.SHORTNAME())
        visit(ctx.equal_part_of_field())
        builder.append(ctx.SEMICOLON())
    }

    override fun visitString_id(ctx: GrammarParser.String_idContext?) {
        ctx ?: return

        visit(ctx.SHORTNAME())
        visit(ctx.STRINGVALUE())

        ctx.PLUS() ?: return

        builder.append(WS).append("+").append(WS)

        visit(ctx.string_id())
    }

    override fun visitBoolean_id(ctx: GrammarParser.Boolean_idContext?) {
        ctx ?: return

        val children = ctx.children

        if (children[0].text == "(") {
            builder.append("(")
            visit(children[1])
            builder.append(")")
            return
        }

        if (children.size == 3) {
            visit(children[0])
            builder.append(WS)
            visit(children[1])
            builder.append(WS)
            visit(children[2])
            return
        }

        children.forEach { visit(it) }
    }

    override fun visitEqual_part_of_field(ctx: GrammarParser.Equal_part_of_fieldContext?) {
        ctx ?: return

        builder.append(WS).append("=").append(WS)
        visit(ctx.value())
    }

    override fun visitValue(ctx: GrammarParser.ValueContext?) {
        ctx ?: return

        visit(ctx.expression())
        visit(ctx.string_id())
        visit(ctx.boolean_id())
        visit(ctx.new_object())
    }

    override fun visitNew_object(ctx: GrammarParser.New_objectContext?) {
        ctx ?: return

        builder.append("new").append(WS)

        visit(ctx.class_name())

        builder.append("(")

        ctx.value().forEachIndexed { index, valueContext ->
            if (index != 0) {
                builder.append(",").append(WS)
            }

            visit(valueContext)
        }

        builder.append(")")
    }

    override fun visitMethod(ctx: GrammarParser.MethodContext?) {
        ctx ?: return

        builder.append(NL).append(NL).append(offset)
        visit(ctx.field_decorators())
        visit(ctx.VOID())
        visit(ctx.type())
        builder.append(WS)
        visit(ctx.SHORTNAME())
        visit(ctx.args_list())
        visit(ctx.method_body())
    }

    override fun visitArgs_list(ctx: GrammarParser.Args_listContext?) {
        ctx ?: return

        builder.append("(")

        val children = ctx.children

        if (children.size > 2) {
            visit(children[1])
            builder.append(WS)
            visit(children[2])

            val size = (children.size - 4) / 3

            for (i in 0 until size) {
                builder.append(",").append(WS)
                visit(ctx.type(i + 1))
                visit(ctx.SHORTNAME(i + 1))
            }
        }

        builder.append(")")
    }

    override fun visitMethod_body(ctx: GrammarParser.Method_bodyContext?) {
        ctx ?: return

        builder.append(WS).append("{").append(NL)
        level++
        firstState = true

        ctx.code_line().forEach {
            visit(it)
            firstState = false
        }

        level--
        builder.append(offset).append("}")
    }

    override fun visitCode_line(ctx: GrammarParser.Code_lineContext?) {
        ctx ?: return

        builder.append(offset)

        val children = ctx.children

        children.forEachIndexed { index, parseTree ->
            if (children.size > 2 && index == 1 && ctx.RETURN() != null) {
                builder.append(WS)
            }

            visit(parseTree)
        }

        builder.append(NL)
    }

    override fun visitStatement(ctx: GrammarParser.StatementContext?) {
        ctx ?: return

        ctx.children ?: return

        ctx.children.forEach { visit(it) }
    }

    override fun visitCreate_variable(ctx: GrammarParser.Create_variableContext?) {
        ctx ?: return

        visit(ctx.FINAL())

        if (ctx.FINAL() != null) {
            builder.append(WS)
            currentType = "final "
        }

        visit(ctx.type())
        builder.append(WS)

        currentType += ctx.type().text

        visit(ctx.SHORTNAME())
        visit(ctx.additional_names())
        visit(ctx.equal_part_of_field())

        currentType = ES
    }

    override fun visitCall_method(ctx: GrammarParser.Call_methodContext?) {
        ctx ?: return

        visit(ctx.SHORTNAME())

        ctx.value().forEachIndexed { index, valueContext ->
            if (index != 0) {
                builder.append(",").append(WS)
            }

            visit(valueContext)
        }
    }

    override fun visitTry_block(ctx: GrammarParser.Try_blockContext?) {
        ctx ?: return

        builder.append("try").append(WS)

        if (ctx.LP() != null) {
            builder.append("(")

            visit(ctx.create_variable())

            builder.append(")")
        }

        visit(ctx.method_body())

        if (ctx.catch_block() != null){
            builder.append(WS)

            visit(ctx.catch_block())
        }

        if (ctx.finally_block() != null) {
            builder.append(WS)

            visit(ctx.finally_block())
        }
    }

    override fun visitFinally_block(ctx: GrammarParser.Finally_blockContext?) {
        ctx ?: return

        builder.append("finally").append(WS)

        visit(ctx.method_body())
    }

    override fun visitCatch_block(ctx: GrammarParser.Catch_blockContext?) {
        ctx ?: return

        builder.append("catch").append(WS).append("(")

        visit(ctx.class_name())
        builder.append(WS)
        visit(ctx.SHORTNAME())

        builder.append(")").append(WS)

        visit(ctx.method_body())
    }

    override fun visitFor_block(ctx: GrammarParser.For_blockContext?) {
        ctx ?: return

        builder.append("for").append(WS).append("(")

        val children = ctx.children

        for (i in 2 until children.size - 2) {
            if (i != 2 && children[i].text != ";" && children[i].text != ":") {
                builder.append(WS)
            }

            visit(children[i])
        }

        builder.append(")")

        visit(ctx.method_body())
    }

    override fun visitWhile_block(ctx: GrammarParser.While_blockContext?) {
        ctx ?: return

        builder.append("while").append(WS).append("(")

        visit(ctx.boolean_id())

        builder.append(")").append(WS)

        visit(ctx.method_body())
    }

    override fun visitDo_while_block(ctx: GrammarParser.Do_while_blockContext?) {
        ctx ?: return

        builder.append("do")

        visit(ctx.method_body())

        builder.append(WS).append("while").append("(")

        visit(ctx.boolean_id())

        builder.append(")")
    }

    override fun visitModifications(ctx: GrammarParser.ModificationsContext?) {
        ctx ?: return

        val static = ctx.STATIC()
        val final = ctx.FINAL()

        if (static != null) {
            builder.append("static").append(WS)
        }

        if (final != null) {
            builder.append("final").append(WS)
        }
    }

    override fun visitAccess_modifiers(ctx: GrammarParser.Access_modifiersContext?) {
        ctx ?: return

        builder.append(ctx.text).append(WS)
    }

    override fun visitClass_name(ctx: GrammarParser.Class_nameContext?) {
        ctx ?: return

        visit(ctx.LONGNAME())
        visit(ctx.SHORTNAME())
    }

    override fun visitExpression(ctx: GrammarParser.ExpressionContext?) {
        ctx ?: return

        val children = ctx.children

        if (children[0].text == "(") {
            builder.append("(")
            visit(children[1])
            builder.append(")")
            return
        }

        if (children[0].text == "+" || children[0].text == "-") {
            builder.append(children[0].text).append(WS)
            visit(children[1])
            return
        }

        if (ctx.INC() != null || ctx.DEC() != null) {
            children.forEach { visit(it) }
            return
        }

        visit(children[0])

        if (children.size > 1) {
            builder.append(WS)
            visit(children[1])
            builder.append(WS)
            visit(children[2])
        }
    }

    override fun visitCondition(ctx: GrammarParser.ConditionContext?) {
        ctx ?: return

        val children = ctx.children

        for (i in 0..2) {
            if (i != 0) {
                builder.append(WS)
            }

            visit(children[i])
        }
    }

    override fun visitCompare_operation(ctx: GrammarParser.Compare_operationContext?) {
        ctx ?: return

        visit(ctx.children[0])
    }

    override fun visitType(ctx: GrammarParser.TypeContext?) {
        ctx ?: return
        visit(ctx.class_name())
        visit(ctx.INT())
        visit(ctx.DOUBLE())
        visit(ctx.STRING())
        visit(ctx.BOOLEAN())
    }

    override fun visitAdditional_names(ctx: GrammarParser.Additional_namesContext?) {
        if (ctx == null) {
            currentType = ES
            return
        }

        ctx.SHORTNAME().forEach { terminalNode ->
            builder.append(";").append("\n").append(offset).append(currentType).append(WS)
            visit(terminalNode)
        }
    }

    override fun visitIf_block(ctx: GrammarParser.If_blockContext?) {
        ctx ?: return

        builder.append("if").append(WS).append("(")
        visit(ctx.boolean_id())
        builder.append(")")

        visit(ctx.method_body())

        ctx.else_if().forEach {
            builder.append(WS)
            visit(it)
        }

        ctx.else_block().apply {
            this ?: return
            builder.append(WS)
            visit(this)
        }
    }

    override fun visitElse_if(ctx: GrammarParser.Else_ifContext?) {
        ctx ?: return

        builder.append("else if (")

        visit(ctx.boolean_id())

        builder.append(")")

        visit(ctx.method_body())
    }

    override fun visitElse_block(ctx: GrammarParser.Else_blockContext?) {
        ctx ?: return

        builder.append("else")

        visit(ctx.method_body())
    }

    override fun visitBlock_of_code(ctx: GrammarParser.Block_of_codeContext?) {
        ctx ?: return

        if (!firstState) {
            builder.append(NL).append(offset)
        }

        visit(ctx.children[0])
    }

    companion object {
        private const val NL = "\n"
        private const val WS = " "
        private const val TAB = "    "
        private const val ES = ""
    }

    override fun visitSwitch_block(ctx: GrammarParser.Switch_blockContext?) {
        ctx ?: return

        builder.append("switch").append(WS).append("(")
        visit(ctx.SHORTNAME())
        builder.append(")").append(WS)
        visit(ctx.case_block())
    }

    override fun visitCase_block(ctx: GrammarParser.Case_blockContext?) {
        ctx ?: return

        builder.append("{").append(NL)
        level++

        ctx.case_condition().forEach {
            builder.append(offset)
            visit(it)
        }

        ctx.default_condition().apply {
            this ?: return
            builder.append(offset)
            visit(this)
        }

        level--
        builder.append(offset).append("}")
    }

    override fun visitCase_condition(ctx: GrammarParser.Case_conditionContext?) {
        ctx ?: return

        builder.append("case").append(WS)
        visit(ctx.value())
        builder.append(":").append(NL)
        level++
        ctx.code_line().forEach { visit(it) }
        level--
    }

    override fun visitDefault_condition(ctx: GrammarParser.Default_conditionContext?) {
        ctx ?: return

        builder.append("default").append(":").append(NL)
        level++
        ctx.code_line().forEach { visit(it) }
        level--
    }
}
