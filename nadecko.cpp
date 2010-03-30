#include <iostream>
#include <iterator>
#include <sstream>
#include <boost/assert.hpp>
#include <boost/range/begin.hpp>
#include <boost/range/end.hpp>
#include <boost/spirit/include/qi.hpp>
#include <boost/spirit/include/qi_parse.hpp>
#include <boost/spirit/include/phoenix_core.hpp>
#include <boost/spirit/include/phoenix_operator.hpp>
#include <boost/spirit/include/phoenix_fusion.hpp>
#include <boost/spirit/include/phoenix_object.hpp>
#include <boost/fusion/include/adapt_struct.hpp>
#include <boost/array.hpp>
#include <boost/variant.hpp>
#include <locale.h>

namespace node
{
    struct terminal { std::wstring value; };

    struct number: terminal {};
    struct string_literal: terminal {};
    struct identifier: terminal {};

    typedef boost::variant<
        number,
        string_literal,
        identifier
        > scalar;

    struct add;
    struct subtract;
    struct multiply;
    struct divide;
    struct assignment;
    struct declaration;

    typedef boost::variant<
        scalar,
        boost::recursive_wrapper<add>,
        boost::recursive_wrapper<subtract>,
        boost::recursive_wrapper<multiply>,
        boost::recursive_wrapper<divide>
        > expr;

    typedef boost::variant<
        expr, 
        declaration,
        boost::recursive_wrapper<assignment>
        > statement;

    typedef std::vector<statement> statements;

    struct binary_op
    {
        expr first;
        expr second;

        binary_op(expr const& first, expr const& second)
            : first(first), second(second) {}
    };

    struct unary_op
    {
        expr first;
    };

#define DECLARE_BINARY_OP_TYPE(op, op_fw, type) \
    struct type: public binary_op \
    { \
        static const wchar_t op_char = op; \
        static const wchar_t op_char_fw = op_fw; \
\
        type(expr const& first, expr const& second) \
            : binary_op(first, second) {} \
    };

    DECLARE_BINARY_OP_TYPE(L'+', L'＋', add)
    DECLARE_BINARY_OP_TYPE(L'-', L'ー', subtract)
    DECLARE_BINARY_OP_TYPE(L'*', L'＊', multiply)
    DECLARE_BINARY_OP_TYPE(L'/', L'／', divide)

    struct assignment
    {
        identifier first;
        expr second;
    };

    struct declaration
    {
        identifier first;
        identifier second;
    };
};

BOOST_FUSION_ADAPT_STRUCT(
    node::number,
    (std::wstring, value)
)

BOOST_FUSION_ADAPT_STRUCT(
    node::string_literal,
    (std::wstring, value)
)

BOOST_FUSION_ADAPT_STRUCT(
    node::identifier,
    (std::wstring, value)
)

BOOST_FUSION_ADAPT_STRUCT(
    node::assignment,
    (node::identifier, first)
    (node::expr, second)
)

BOOST_FUSION_ADAPT_STRUCT(
    node::declaration,
    (node::identifier, first)
    (node::identifier, second)
)

BOOST_FUSION_ADAPT_STRUCT(
    node::add,
    (node::expr, first)
    (node::expr, second)
)

BOOST_FUSION_ADAPT_STRUCT(
    node::subtract,
    (node::expr, first)
    (node::expr, second)
)

BOOST_FUSION_ADAPT_STRUCT(
    node::multiply,
    (node::expr, first)
    (node::expr, second)
)

BOOST_FUSION_ADAPT_STRUCT(
    node::divide,
    (node::expr, first)
    (node::expr, second)
)

struct my_parser_node_printer: public boost::static_visitor<>
{
    my_parser_node_printer(std::wostream& out): out_(out) {}

    void operator()(node::assignment const& n) const
    {
        out_ << L"(assignment ";
        (*this)(n.first);
        out_ << L" ";
        boost::apply_visitor(*this, n.second);
        out_ << L")";
    }

    void operator()(node::declaration const& n) const
    {
        out_ << L"(declaration ";
        (*this)(n.first);
        out_ << L" ";
        (*this)(n.second);
        out_ << L")";
    }

    void operator()(node::expr const& n) const
    {
        out_ << L"(expr ";
        boost::apply_visitor(*this, n);
        out_ << L")";
    }

#define BINARY_OP_VISITOR(x) \
    void operator()(node::x const& n) const \
    { \
        out_ << L"(" #x " "; \
        boost::apply_visitor(*this, n.first); \
        out_ << L" "; \
        boost::apply_visitor(*this, n.second); \
        out_ << L")"; \
    }

    BINARY_OP_VISITOR(add)
    BINARY_OP_VISITOR(subtract)
    BINARY_OP_VISITOR(multiply)
    BINARY_OP_VISITOR(divide)

#undef BINARY_OP_VISITOR

    void operator()(node::scalar const& n) const
    {
        boost::apply_visitor(*this, n);
    }

    void operator()(node::number const& n) const
    {
        out_ << L"(number \"" << n.value << "\")";
    }

    void operator()(node::string_literal const& n) const
    {
        out_ << L"(string_literal \"" << n.value << "\")";
    }

    void operator()(node::identifier const& n) const
    {
        out_ << L"(identifier \"" << n.value << "\")";
    }

    std::wostream& out_;
};

struct my_skipper: public boost::spirit::qi::primitive_parser<my_skipper>
{
    template<typename Tctx_, typename Titer_>
    struct attribute
    {
        typedef boost::spirit::unused_type type;
    };

    template<typename Titer_, typename Tctx_, typename Tskipper_, typename Tattr_>
    bool parse(Titer_& first, Titer_ const& last, Tctx_& ctx, Tskipper_ const&,
               Tattr_&) const
    {
        if (*first == L' ' || *first == L'　' || *first == L'\t')
        {
            ++first;
            return true;
        }
        return false;
    }
};

template<typename Titer_, typename Tskipper_>
struct my_grammar
    : boost::spirit::qi::grammar<Titer_, typename node::statements(), Tskipper_>
{
    struct range_assigner
    {
        template<typename Titer__>
        static void assign(std::wstring& target, Titer__ const& b, Titer__ const& e)
        {
            target.assign(b, e);
        }

        template<typename Titer__>
        static void assign(node::terminal& target, Titer__ const& b, Titer__ const& e)
        {
            assign(target.value, b, e);
        }

        template<typename Tattr_, typename Tctx_>
        void operator()(Tattr_ const& a, Tctx_ const& ctx, boost::spirit::unused_type) const
        {
            using boost::fusion::at_c;
            assign(at_c<0>(ctx.attributes), boost::begin(a), boost::end(a));
        }
    };

    struct build_op
    {
        template<typename Tresult_, typename Tnode_>
        struct appender
        {
            appender(Tresult_& result, Tnode_ const& first)
                : next_(result)
            {
                next_ = first;
            }

            template<typename Tpair_>
            void operator()(Tpair_ const& pair)
            {
                using boost::fusion::at_c;
                switch (at_c<0>(pair))
                {
#define BINARY_OP_HANDLER(type) \
                case node::type::op_char: case node::type::op_char_fw: \
                    { \
                        next_ = node::type(next_, at_c<1>(pair)); \
                    } \
                    break;
                BINARY_OP_HANDLER(add)
                BINARY_OP_HANDLER(subtract)
                BINARY_OP_HANDLER(multiply)
                BINARY_OP_HANDLER(divide)
#undef BINARY_OP_HANDLER
                default:
                    BOOST_ASSERT(false);
                }
            }

            Tresult_& next_;
        };

        template<typename Tresult_, typename Tnode_>
        static appender<Tresult_, Tnode_> make_appender(Tresult_& result, Tnode_ const& first)
        {
            return appender<Tresult_, Tnode_>(result, first);
        }

        template<typename Tattr_, typename Tctx_>
        void operator()(Tattr_ const& attr, Tctx_ const& ctx, boost::spirit::unused_type) const
        {
            using boost::fusion::at_c;
            std::for_each(boost::begin(at_c<1>(attr)), boost::end(at_c<1>(attr)), make_appender(at_c<0>(ctx.attributes), at_c<0>(attr)));
        }
    };


    my_grammar(): my_grammar::base_type(statements)
    {
        using namespace boost::spirit::qi::labels; // for _val and _1
        using boost::spirit::qi::lexeme;
        using boost::spirit::standard_wide::char_;
        using boost::spirit::standard_wide::string;
        using boost::spirit::lit;
        using boost::spirit::omit;
        using boost::spirit::eol;
        using boost::phoenix::at_c;
        using boost::phoenix::construct;

        eos = (!char_(L",、，") >> omit[eol])|omit[char_(L"。")];

        statements %= omit[*eos] >> *(statement >> omit[+eos]);

        statement %= assignment | declaration | expr;

        expr = (term >> *(char_(L"+＋−－-") >> term)) [build_op()];

        term = (scalar >> *(char_(L"/*／＊") >> scalar)) [build_op()];

        scalar %= number | string_literal | identifier;

        number = lexeme[+char_(L"0-9０-９")] [range_assigner()];

        string_literal = lexeme[L'「' >> (*(~char_(L'」')) [at_c<0>(_val) += _1]) >> L'」'];

#define NO_IDENT_CHARS L"かとにはでを「」、,，。+＋/*／＊−－\r\n"
        identifier = lexeme[((~char_(NO_IDENT_CHARS L"0-9０−９-")) [at_c<0>(_val) += _1] >> *((~char_(NO_IDENT_CHARS)) [at_c<0>(_val) += _1])) | (char_(L'か') [at_c<0>(_val) += _1] >> !char_(L'ら'))];
#undef NO_IDENT_CHARS

        assignment %= identifier >> omit[char_(L"は＝")] >> expr;

        declaration %= identifier >> L"とは" >> identifier;
    }

    boost::spirit::qi::rule<Titer_, boost::spirit::unused_type, Tskipper_> eos;
    boost::spirit::qi::rule<Titer_, node::statements(), Tskipper_> statements;
    boost::spirit::qi::rule<Titer_, node::statement(), Tskipper_> statement;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> expr;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> term;
    boost::spirit::qi::rule<Titer_, node::scalar(), Tskipper_> scalar;
    boost::spirit::qi::rule<Titer_, node::assignment(), Tskipper_> assignment;
    boost::spirit::qi::rule<Titer_, node::declaration(), Tskipper_> declaration;
    boost::spirit::qi::rule<Titer_, node::number(), Tskipper_> number;
    boost::spirit::qi::rule<Titer_, node::string_literal(), Tskipper_> string_literal;
    boost::spirit::qi::rule<Titer_, node::identifier(), Tskipper_> identifier;
};

int main()
{
    static const my_grammar<std::wstring::const_iterator, my_skipper> grammar;
    using namespace boost::spirit;

    node::statements statements;
    setlocale(LC_ALL, ""); // workaround: std::locale::global(std::locale(""))
    std::wstring buf;
    typedef std::istream_iterator<wchar_t, wchar_t> wistream_iterator;
    std::wcin >> std::noskipws;
    std::copy(wistream_iterator(std::wcin), wistream_iterator(),
            std::back_inserter<std::wstring>(buf));
    std::wstring::const_iterator b(buf.begin()), e(buf.end());
    qi::phrase_parse(b, e, grammar, my_skipper(), statements);
    BOOST_FOREACH(node::statement const& statement, statements)
    {
        boost::apply_visitor(my_parser_node_printer(std::wcout), statement);
        std::wcout << L'\n';
    }
}
