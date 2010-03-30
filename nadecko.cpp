#include <iostream>
#include <iterator>
#include <sstream>
#include <boost/assert.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/is_base_of.hpp>
#include <boost/range/begin.hpp>
#include <boost/range/end.hpp>
#include <boost/range/const_iterator.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/algorithm/string/find.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/spirit/include/version.hpp>
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

#if SPIRIT_VERSION < 0x2010
#error This code requires a newer version of Boost.Spirit
struct spirit_version_ ## SPIRIT_VERSION;
const int _ = sizeof(spirit_version_ ## SPIRIT_VERSION)
#endif

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
    struct modulo;
    struct concatenate;
    struct greater;
    struct less;
    struct greater_equal;
    struct less_equal;
    struct assignment;
    struct declaration;

    typedef boost::variant<
        scalar,
        boost::recursive_wrapper<add>,
        boost::recursive_wrapper<subtract>,
        boost::recursive_wrapper<multiply>,
        boost::recursive_wrapper<divide>,
        boost::recursive_wrapper<modulo>,
        boost::recursive_wrapper<greater>,
        boost::recursive_wrapper<less>,
        boost::recursive_wrapper<greater_equal>,
        boost::recursive_wrapper<less_equal>,
        boost::recursive_wrapper<concatenate>
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

        binary_op() {}

        binary_op(expr const& first, expr const& second)
            : first(first), second(second) {}
    };

    struct unary_op
    {
        expr first;
    };

#define DECLARE_BINARY_OP_TYPE(type) \
    struct type: binary_op \
    { \
        type(): binary_op() {} \
    \
        type(expr const& first, expr const& second) \
            : binary_op(first, second) {} \
    };

    DECLARE_BINARY_OP_TYPE(add)
    DECLARE_BINARY_OP_TYPE(subtract)
    DECLARE_BINARY_OP_TYPE(multiply)
    DECLARE_BINARY_OP_TYPE(divide)
    DECLARE_BINARY_OP_TYPE(modulo)
    DECLARE_BINARY_OP_TYPE(greater)
    DECLARE_BINARY_OP_TYPE(less)
    DECLARE_BINARY_OP_TYPE(greater_equal)
    DECLARE_BINARY_OP_TYPE(less_equal)
    DECLARE_BINARY_OP_TYPE(concatenate)

#undef DECLARE_BINARY_OP_TYPE

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

struct my_parser_node_printer: boost::static_visitor<>
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
    BINARY_OP_VISITOR(modulo)
    BINARY_OP_VISITOR(greater)
    BINARY_OP_VISITOR(less)
    BINARY_OP_VISITOR(greater_equal)
    BINARY_OP_VISITOR(less_equal)
    BINARY_OP_VISITOR(concatenate)

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

struct my_skipper: boost::spirit::qi::primitive_parser<my_skipper>
{
    template<typename Tctx_, typename Titer_>
    struct attribute
    {
        typedef boost::spirit::unused_type type;
    };

    template<typename Titer_, typename Tctx_, typename Tskipper_, typename Tattr_>
    bool parse(Titer_& first, Titer_ const& last, Tctx_& ctx, Tskipper_ const&, Tattr_&) const
    {
        if (*first == L' ' || *first == L'　' || *first == L'\t')
        {
            ++first;
            return true;
        }
        return false;
    }

    template<typename Tctx_>
    boost::spirit::info what(Tctx_ const&) const
    {
        return boost::spirit::info();
    }
};

#define KANJI_DIGITS L"〇一二三四五六七八九"
#define FULLWIDTH_DIGITS L"０１２３４５６７８９"
#define HALFWIDTH_DIGITS L"0123456789"
#define NO_IDENT_CHARS L"かとにはでを「」、,，。+＋/／*＊%％&＆−－>＞<＜=＝\r\n"

struct kanji_number_parser: boost::spirit::qi::primitive_parser<kanji_number_parser>
{
    template<typename Tctx_, typename Titer_>
    struct attribute
    {
        typedef node::number type;
    };

    template<typename Titer_, typename Tctx_, typename Tskipper_, typename Tattr_>
    bool parse(Titer_& first, Titer_ const& last, Tctx_& ctx, Tskipper_ const&, Tattr_& attr) const
    {
        Titer_ orig(first);
        static wchar_t const group_names[] = L"兆億万";
        std::wostringstream out;
        out << std::dec;
        out << std::setfill(L'0');
        wchar_t const* i(group_names);
        bool groups_exist(false);
        while (first != last)
        {
            int n(parse_basic_group(first, last));
            if (n == 0)
            {
                if (!parse_natural_number(first, last, n))
                {
                    if (!groups_exist)
                    {
                        first = orig;
                        return false;
                    }
                }
            }
            wchar_t const* o(i);
            if (first != last)
            {
                while (*i && *i != *first)
                {
                    ++i;
                }
            }
            else
            {
                while (*i) { ++i; }
            }
            if (groups_exist)
            {
                while (o < i)
                {
                    out << std::setw(4) << 0;
                    ++o;
                }
            }
            out << std::setw(groups_exist ? 4: 0) << n;
            if (n == 0 || !*i)
            {
                break;
            }
            ++i;
            ++first;
            groups_exist = true;
        }
        attr.value = out.str();
        return true;
    }

    template<typename Titer_>
    static bool parse_natural_number(Titer_& first, Titer_ const& last, int& n)
    {
        int _n = parse_single_arabic_number(first, last);
        if (_n == -1)
        {
            return false;
        }
        for (;;)
        {
            const int d(parse_single_arabic_number(first, last));
            if (d == -1)
            {
                break;
            }
            _n = _n * 10 + d;
        }
        n = _n;
        return true;
    }

    template<typename Titer_>
    static int parse_single_arabic_number(Titer_& first, Titer_ const& last)
    {
        int n(0);
        n = parse_single_arabic_number_halfwidth(first, last);
        if (n >= 0)
        {
            return n;
        }
        return parse_single_arabic_number_fullwidth(first, last);
    }

    template<typename Titer_>
    static int parse_single_arabic_number_fullwidth(Titer_& first, Titer_ const& last)
    {
        wchar_t const* r(std::find(boost::begin(fullwidth_digits), boost::end(fullwidth_digits) - 1, *first));
        if (r == boost::end(fullwidth_digits) - 1)
        {
            return -1;
        }
        ++first;
        return (r - boost::begin(fullwidth_digits));
    }

    template<typename Titer_>
    static int parse_single_arabic_number_halfwidth(Titer_& first, Titer_ const& last)
    {
        wchar_t const* r(std::find(boost::begin(halfwidth_digits), boost::end(halfwidth_digits) - 1, *first));
        if (r == boost::end(halfwidth_digits) - 1)
        {
            return -1;
        }
        ++first;
        return (r - boost::begin(halfwidth_digits));
    }

    template<typename Titer_>
    static int parse_single_kanji_digit(Titer_& first, Titer_ const& last)
    {
        wchar_t const* r(std::find(boost::begin(kanji_digits), boost::end(kanji_digits) - 1, *first));
        if (r == boost::end(kanji_digits) - 1)
        {
            return -1;
        }
        ++first;
        return (r - boost::begin(kanji_digits));
    }

    template<typename Titer_>
    static int parse_basic_group(Titer_& first, Titer_ const& last)
    {
        const int thousands(parse_thousands_group(first, last));
        if (first == last)
        {
            return thousands;
        }
        const int hundreds(parse_hundreds_group(first, last));
        if (first == last)
        {
            return thousands + hundreds;
        }
        const int tens(parse_tens_group(first, last));
        if (first == last)
        {
            return thousands + hundreds + tens;
        }
        int n(0);
        {
            Titer_ orig(first);
            n = parse_single_kanji_digit(first, last);
            if (n <= 0)
            {
                n = 0;
                first = orig;
            }
        }

        return thousands + hundreds + tens + n;
    }

    template<typename Titer_>
    static int parse_tens_group(Titer_& first, Titer_ const& last)
    {
        const Titer_ orig(first);
        const int digit(parse_single_kanji_digit(first, last));

        if (digit == 0 || first == last || *first != L'十')
        {
            first = orig;
            return 0;
        }
        ++first;
        return (digit > 0 ? digit: 1) * 10;
    }

    template<typename Titer_>
    static int parse_hundreds_group(Titer_& first, Titer_ const& last)
    {
        const Titer_ orig(first);
        const int digit(parse_single_kanji_digit(first, last));

        if (digit == 0 || first == last || *first != L'百')
        {
            first = orig;
            return 0;
        }
        ++first;
        return (digit > 0 ? digit: 1) * 100;
    }

    template<typename Titer_>
    static int parse_thousands_group(Titer_& first, Titer_ const& last)
    {
        const Titer_ orig(first);
        const int digit(parse_single_kanji_digit(first, last));

        if (digit == 0 || first == last || *first != L'千')
        {
            first = orig;
            return 0;
        }
        ++first;
        return (digit > 0 ? digit: 1) * 1000;
    }

    template<typename Tctx_>
    boost::spirit::info what(Tctx_ const&) const
    {
        return boost::spirit::info("(kanji number)");
    }

    static wchar_t const kanji_digits[11];
    static wchar_t const halfwidth_digits[11];
    static wchar_t const fullwidth_digits[11];
};

wchar_t const kanji_number_parser::kanji_digits[] = KANJI_DIGITS;
wchar_t const kanji_number_parser::halfwidth_digits[] = HALFWIDTH_DIGITS;
wchar_t const kanji_number_parser::fullwidth_digits[] = FULLWIDTH_DIGITS;

template<typename Titer_, typename Tskipper_>
struct my_grammar
    : boost::spirit::qi::grammar<Titer_, typename node::statements(), Tskipper_>
{

    typedef typename boost::iterator_value<Titer_>::type char_type;

    struct string_appender
    {
        template<typename Tresult_>
        struct appender
        {
            appender(Tresult_& result): result_(result) {}

            template<typename Tchar_>
            void operator()(Tchar_ const& c) const
            {
                append(result_, c);
            }

            Tresult_& result_;
        };

        template<typename Ttarget_, typename Tchar_>
        static void append(Ttarget_& target, Tchar_ const& c, typename boost::disable_if<boost::is_base_of<node::terminal, Ttarget_> >::type* = 0)
        {
            target.push_back(c);
        }

        template<typename Tchar_>
        static void append(node::terminal& target, Tchar_ const& c)
        {
            append(target.value, c);
        }

        template<typename Ttarget_, typename Titer__>
        static void append(Ttarget_& target, Titer__ const& b, Titer__ const& e, typename boost::disable_if<boost::is_base_of<node::terminal, Ttarget_> >::type* = 0)
        {
            target.append(b, e);
        }

        template<typename Titer__>
        static void append(node::terminal& target, Titer__ const& b, Titer__ const& e)
        {
            append(target.value, b, e);
        }

        template<typename Tresult_>
        static appender<Tresult_> make_appender(Tresult_& result)
        {
            return appender<Tresult_>(result);
        }

        template<typename Tattr_, typename Tctx_>
        void operator()(Tattr_ const& a, Tctx_ const& ctx, boost::spirit::unused_type, typename boost::disable_if<boost::mpl::or_<boost::fusion::traits::is_sequence<Tattr_>, boost::is_same<Tattr_, char_type> > >::type* = 0) const
        {
            append(boost::fusion::at_c<0>(ctx.attributes), boost::begin(a), boost::end(a));
        }

        template<typename Tattr_, typename Tctx_>
        void operator()(Tattr_ const& a, Tctx_ const& ctx, boost::spirit::unused_type, typename boost::enable_if<boost::is_same<Tattr_, char_type> >::type* = 0) const
        {
            append(boost::fusion::at_c<0>(ctx.attributes), a);
        }

        template<typename Tattr_, typename Tctx_>
        void operator()(Tattr_ const& a, Tctx_ const& ctx, boost::spirit::unused_type, typename boost::enable_if<boost::fusion::traits::is_sequence<Tattr_> >::type* = 0) const
        {
            boost::fusion::for_each(a, make_appender(boost::fusion::at_c<0>(ctx.attributes)));
        }
    };

    struct range_seq_appender
    {
        template<typename Tresult_>
        struct appender
        {
            appender(Tresult_& result): result_(result) {}

            template<typename Trange_>
            void operator()(Trange_ const& r) const
            {
                string_appender::append(result_, boost::begin(r), boost::end(r));
            }

            Tresult_& result_;
        };

        template<typename Tresult_>
        static appender<Tresult_> make_appender(Tresult_& result)
        {
            return appender<Tresult_>(result);
        }

        template<typename Tattr_, typename Tctx_>
        void operator()(Tattr_ const& a, Tctx_ const& ctx, boost::spirit::unused_type) const
        {
            boost::fusion::for_each(a, make_appender(boost::fusion::at_c<0>(ctx.attributes)));
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

            template<typename Tpat_, typename Trange_>
            static bool equals(Tpat_ const& pat, Trange_ const& r)
            {
                typename boost::range_const_iterator<Tpat_>::type const
                    pe(boost::end(pat));
                typename boost::range_const_iterator<Trange_>::type const
                    e(boost::end(r));
                typename boost::range_const_iterator<Trange_>::type
                        i(boost::begin(r));
                for (typename boost::range_const_iterator<Tpat_>::type
                    pi(boost::begin(pat));; ++pi)
                {
                    if (pi == pe || *pi == L'|')
                    {
                        if (i == e)
                            break;

                        if (pi == pe)
                            return false;
                        else
                            continue;
                    }

                    if (i == e || *pi != *i)
                    {
                        pi = std::find(pi, pe, L'|');
                        if (pi == pe)
                            return false;

                        i = boost::begin(r);
                    }
                    else
                    {
                        ++i;
                    }
                }
                return true;
            }

            template<typename Trange_, typename Tnext_node_>
            void operator()(Trange_ const& r, Tnext_node_ const& node, typename boost::disable_if<boost::is_same<Trange_, char_type> >::type* = 0)
            {
#define BINARY_OP_HANDLER(_pat, type) \
                { \
                    static wchar_t const pat[] = _pat; \
                    if (equals(boost::make_iterator_range(boost::begin(pat), boost::end(pat) - 1), r)) \
                    { \
                        next_ = node::type(next_, node); \
                        return; \
                    } \
                }
                BINARY_OP_HANDLER(L"+|＋", add)
                BINARY_OP_HANDLER(L"−|－|-", subtract)
                BINARY_OP_HANDLER(L"*|＊", multiply)
                BINARY_OP_HANDLER(L"/|／", divide)
                BINARY_OP_HANDLER(L"%|％", modulo)
                BINARY_OP_HANDLER(L">=|＞＝", greater_equal)
                BINARY_OP_HANDLER(L"<=|＜＝", less_equal)
                BINARY_OP_HANDLER(L">|＞", greater)
                BINARY_OP_HANDLER(L"<|＜", less)
                BINARY_OP_HANDLER(L"&|＆", concatenate)
#undef BINARY_OP_HANDLER
                BOOST_ASSERT(false && "should never get here");
            }

            template<typename Tchar_, typename Tnext_node_>
            void operator()(Tchar_ const& c, Tnext_node_ const& node, typename boost::enable_if<boost::is_same<Tchar_, char_type> >::type* = 0)
            {
                Tchar_ const str[] = { c };
                (*this)(str, node);
            }

            template<typename Tpair_>
            void operator()(Tpair_ const& pair)
            {
                using boost::fusion::at_c;
                (*this)(at_c<0>(pair), at_c<1>(pair));
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

        eos = (!char_(L",、，") >> omit[eol])|omit[char_(L"。")];

        statements %= omit[*eos] >> *(statement >> omit[+eos]);

        statement %= assignment | declaration | expr;

        expr %= greater_less;

        greater_less = (concat >> *((string(L">=")|string(L"＞＝")|string(L"<=")|string(L"＜＝")|string(L">")|string(L"＞")|string(L"<")|string(L"＜")) >> concat)) [build_op()];

        concat = (add_sub >> *(char_(L"&＆") >> add_sub)) [build_op()];

        add_sub = (mod >> *(char_(L"+＋−－-") >> mod)) [build_op()];

        mod = (mul_div >> *(char_(L"%％") >> mul_div)) [build_op()];

        mul_div = (scalar >> *(char_(L"/／*＊") >> scalar)) [build_op()];

        scalar %= number | string_literal | identifier;

        string_literal = lexeme[L'「' >> *((~char_(L"」\\")) [string_appender()]|(char_(L'\\') >> char_) [string_appender()]) >> L'」']
                        | lexeme[L'"' >> *((~char_(L"\"\\")) [at_c<0>(_val) += _1]|(char_(L'\\') >> char_) [string_appender()]) >> L'"']
                        | lexeme[char_(L"“”") >> *((~char_(L"“”\\")) [string_appender()] |(char_(L'\\') >> char_) [string_appender()]) >> L'”'];

        identifier = lexeme[((~char_(NO_IDENT_CHARS HALFWIDTH_DIGITS FULLWIDTH_DIGITS KANJI_DIGITS)) [string_appender()] >> *((~char_(NO_IDENT_CHARS)) [string_appender()])) | (char_(L'か') [string_appender()] >> !char_(L'ら'))];

        assignment %= identifier >> omit[char_(L"は=＝")] >> expr;

        declaration %= identifier >> L"とは" >> identifier;
    }

    boost::spirit::qi::rule<Titer_, boost::spirit::unused_type, Tskipper_> eos;
    boost::spirit::qi::rule<Titer_, node::statements(), Tskipper_> statements;
    boost::spirit::qi::rule<Titer_, node::statement(), Tskipper_> statement;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> expr;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> greater_less;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> concat;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> add_sub;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> mod;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> mul_div;
    boost::spirit::qi::rule<Titer_, node::scalar(), Tskipper_> scalar;
    boost::spirit::qi::rule<Titer_, node::assignment(), Tskipper_> assignment;
    boost::spirit::qi::rule<Titer_, node::declaration(), Tskipper_> declaration;
    kanji_number_parser number;
    boost::spirit::qi::rule<Titer_, node::string_literal(), Tskipper_> string_literal;
    boost::spirit::qi::rule<Titer_, node::identifier(), Tskipper_> identifier;
};
#undef FULLWIDTH_DIGITS
#undef HALFWIDTH_DIGITS
#undef KANJI_DIGITS
#undef NO_IDENT_CHARS

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
