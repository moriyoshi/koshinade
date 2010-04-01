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
#include <boost/algorithm/string/predicate.hpp>
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

enum particle_type
{
    PARTICLE_TYPE_TO = 0,
    PARTICLE_TYPE_NI,
    PARTICLE_TYPE_HA,
    PARTICLE_TYPE_DE,
    PARTICLE_TYPE_WO,
    PARTICLE_TYPE_HE,
    PARTICLE_TYPE_TOWA,
    PARTICLE_TYPE_KARA,
    PARTICLE_TYPE_MADE
};

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
    struct sentence;

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
        boost::recursive_wrapper<concatenate>,
        boost::recursive_wrapper<sentence>
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

    struct objective_pair
    {
        expr first;
        enum particle_type second;
    };

    struct sentence
    {
        std::vector<objective_pair> objects;
        identifier verb;
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
    node::objective_pair,
    (node::expr, first)
    (enum particle_type, second)
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

    void operator()(node::objective_pair const& n) const
    {
        out_ << L"(objective_pair ";
        (*this)(n.first);
        out_ << L" ";
        (*this)(n.second);
        out_ << L")";
    }

    void operator()(enum particle_type const& n) const
    {
        switch (n)
        {
        case PARTICLE_TYPE_TO: out_ << L":to"; break;
        case PARTICLE_TYPE_NI: out_ << L":ni"; break;
        case PARTICLE_TYPE_HA: out_ << L":ha"; break;
        case PARTICLE_TYPE_DE: out_ << L":de"; break;
        case PARTICLE_TYPE_WO: out_ << L":wo"; break;
        case PARTICLE_TYPE_HE: out_ << L":he"; break;
        case PARTICLE_TYPE_TOWA: out_ << L":towa"; break;
        case PARTICLE_TYPE_KARA: out_ << L":kara"; break;
        case PARTICLE_TYPE_MADE: out_ << L":made"; break;
        }
    }

    void operator()(node::sentence const& n) const
    {
        out_ << L"(sentence ";
        BOOST_FOREACH (node::objective_pair const& pair, n.objects)
        {
            (*this)(pair);
        }
        out_ << L" ";
        (*this)(n.verb);
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

static inline bool is_comma(wchar_t c)
{
    return c == L',' || c == L'，' || c == L'、' || c == L'､';
}

struct comma_skipper: boost::spirit::qi::primitive_parser<comma_skipper>
{
    template<typename Tctx_, typename Titer_>
    struct attribute
    {
        typedef boost::spirit::unused_type type;
    };

    template<typename Titer, typename Tctx, typename Tskipper, typename Tattr>
    bool parse(Titer& first, Titer const& last, Tctx& ctx, Tskipper const&, Tattr&) const
    {
        if (first != last && is_comma(*first))
        {
            ++first;
        }
        return true;
    }

    template<typename Tctx>
    boost::spirit::info what(Tctx const&) const
    {
        return boost::spirit::info();
    }
};

struct my_skipper: boost::spirit::qi::primitive_parser<my_skipper>
{
    template<typename Tctx_, typename Titer_>
    struct attribute
    {
        typedef boost::spirit::unused_type type;
    };

    template<typename Titer, typename Tctx, typename Tskipper, typename Tattr>
    bool parse(Titer& first, Titer const& last, Tctx& ctx, Tskipper const&, Tattr&) const
    {
        if (*first == L' ' || *first == L'　' || *first == L'\t')
        {
            ++first;
            return true;
        }
        else
        {
            const Titer orig(first);
            if (is_comma(*first))
            {
                ++first;
                if (first != last)
                {
                    if (*first == L'\n')
                    {
                        ++first;
                    }
                    else if (*first == L'\r')
                    {
                        ++first;
                        if (first != last && *first == L'\n')
                            ++first;
                    }
                }
                else
                {
                    first = orig;
                }
            }
        }

        return false;
    }

    template<typename Tctx>
    boost::spirit::info what(Tctx const&) const
    {
        return boost::spirit::info();
    }
};

struct my_grammar_parsers
{
    struct kanji_number_parser: boost::spirit::qi::primitive_parser<kanji_number_parser>
    {
        template<typename Tctx_, typename Titer_>
        struct attribute
        {
            typedef node::number type;
        };

        template<typename Titer, typename Tctx, typename Tskipper, typename Tattr>
        bool parse(Titer& first, Titer const& last, Tctx& ctx, Tskipper const&, Tattr& attr) const
        {
            std::wostringstream out;
            if (!parse_kanji_number(first, last, out))
            {
                if (first == last)
                    return false;
                if (*first != L'.' && *first != L'．')
                    return false;
            }
            if (first != last && (*first == L'.' || *first == L'．'))
            {
                ++first;
                {
                    const int n(parse_single_arabic_digit(first, last));
                    if (n == -1)
                    {
                        // dot as a separate token
                        attr.value = out.str();
                        return true;
                    }
                    out << L'.';
                    out << n;
                }
                for (;;)
                {
                    const int n(parse_single_arabic_digit(first, last));
                    if (n == -1)
                        break;
                    out << n;
                }
            }
            if (first != last && (*first == L'e' || *first == L'ｅ' || *first == L'E' || *first == L'Ｅ'))
            {
                const Titer orig(first);
                ++first;
                if (first == last)
                {
                    first = orig;
                }
                else
                {
                    bool sign(false);
                    if (*first == L'-' || *first == L'−' || * first == L'－')
                    {
                        ++first;
                        sign = true;
                    }
                    if (first != last)
                    {
                        int n(parse_single_arabic_digit(first, last));
                        if (n >= 0)
                        {
                            out << L'e';
                            if (sign)
                            {
                                out << L'-';
                            }
                            while (n >= 0)
                            {
                                out << n;
                                n = parse_single_arabic_digit(first, last);
                            }
                        }
                        else
                        {
                            first = orig;
                        }
                    }
                    else
                    {
                        first = orig;
                    }
                }
            }
            attr.value = out.str();
            return true;
        }

        template<typename Titer>
        bool parse_kanji_number(Titer& first, Titer const& last, std::wostringstream& out) const
        {
            const Titer orig(first);
            static wchar_t const group_names[] = L"兆億万";
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
            return true;
        }

        template<typename Titer>
        static bool parse_natural_number(Titer& first, Titer const& last, int& n)
        {
            int _n = parse_single_arabic_digit(first, last);
            if (_n == -1)
            {
                return false;
            }
            for (;;)
            {
                const int d(parse_single_arabic_digit(first, last));
                if (d == -1)
                {
                    break;
                }
                _n = _n * 10 + d;
            }
            n = _n;
            return true;
        }

        template<typename Titer>
        static int parse_basic_group(Titer& first, Titer const& last)
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
                Titer orig(first);
                n = parse_single_kanji_digit(first, last);
                if (n <= 0)
                {
                    n = 0;
                    first = orig;
                }
                else if (thousands == 0 && hundreds == 0 && tens == 0)
                {
                    for (;;)
                    {
                        if (first == last)
                            break;
                        const int _n(parse_single_kanji_digit(first, last));
                        if (_n < 0)
                            break;
                        n = n * 10 + _n;
                    }
                }
            }

            return thousands + hundreds + tens + n;
        }

        template<typename Titer>
        static int parse_tens_group(Titer& first, Titer const& last)
        {
            const Titer orig(first);
            const int digit(parse_single_kanji_digit(first, last));

            if (digit == 0 || first == last || *first != L'十')
            {
                first = orig;
                return 0;
            }
            ++first;
            return (digit > 0 ? digit: 1) * 10;
        }

        template<typename Titer>
        static int parse_hundreds_group(Titer& first, Titer const& last)
        {
            const Titer orig(first);
            const int digit(parse_single_kanji_digit(first, last));

            if (digit == 0 || first == last || *first != L'百')
            {
                first = orig;
                return 0;
            }
            ++first;
            return (digit > 0 ? digit: 1) * 100;
        }

        template<typename Titer>
        static int parse_thousands_group(Titer& first, Titer const& last)
        {
            const Titer orig(first);
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
            return boost::spirit::info("(number)");
        }
    };

    struct identifier_parser: boost::spirit::qi::primitive_parser<identifier_parser>
    {
        template<typename Tctx_, typename Titer_>
        struct attribute
        {
            typedef std::wstring type;
        };

        template<typename Titer, typename Tctx, typename Tskipper, typename Tattr>
        bool parse(Titer& first, Titer const& last, Tctx& ctx, Tskipper const&, Tattr& attr) const
        {
            if (first == last)
                return false;

            {
                const Titer orig(first);
                if (parse_single_digit(first, last) >= 0)
                {
                    first = orig;
                    return false;
                }
            }

            if (is_cjk_ideograph(*first))
            {
                {
                    const Titer orig(first);
                    if (parse_conjuction(first, last) >= 0)
                    {
                        first = orig;
                        return false;
                    }
                }

                attr.push_back(*first);
                ++first;

                if (first == last)
                    return true;

                if (is_hiragana(*first))
                {
                    while (first != last)
                    { 
                        if (!is_hiragana(*first))
                            return true;
                        if (reserved_occurs_ahead(first, last))
                            return true;
                        ++first;
                    }
                }
                else
                {
                    while (first != last)
                    { 
                        if (is_no_ident_char(*first))
                            return true;
                        if (reserved_occurs_ahead(first, last))
                            return true;
                        attr.push_back(*first);
                        ++first;
                    }
                }
            }
            else
            {
                if (is_no_ident_char(*first))
                    return false;
                if (reserved_occurs_ahead(first, last))
                    return false;
                attr.push_back(*first);
                ++first;

                while (first != last)
                {
                    if (is_no_ident_char(*first))
                        return true;
                    if (reserved_occurs_ahead(first, last))
                        return true;
                    attr.push_back(*first);
                    ++first;
                }
            }
            return true;
        }
        
        template<typename Tctx_>
        boost::spirit::info what(Tctx_ const&) const
        {
            return boost::spirit::info("(identifier)");
        }
    };

    template<typename Tverb_collection_>
    struct symbol_parser: boost::spirit::qi::primitive_parser<symbol_parser<Tverb_collection_> >
    {
        template<typename Tctx_, typename Titer_>
        struct attribute
        {
            typedef node::identifier type;
        };

        template<typename Titer, typename Tctx, typename Tskipper, typename Tattr>
        bool parse(Titer& first, Titer const& last, Tctx& ctx, Tskipper const& skipper, Tattr& attr) const
        {
            const Titer orig(first);
            typename identifier_parser::attribute<Tctx, Titer>::type result;

            if (!inner.parse(first, last, ctx, skipper, result))
            {
                return false;
            }

            BOOST_FOREACH (
                typename boost::range_value<Tverb_collection_>::type const& w,
                verbs)
            {
                if (boost::algorithm::equals(w, result))
                {
                    first = orig;
                    return false;
                }
            }

            attr.value = result;

            return true;
        }
        
        template<typename Tctx>
        boost::spirit::info what(Tctx const&) const
        {
            return boost::spirit::info("(symbol)");
        }

        symbol_parser(Tverb_collection_ const& verbs): verbs(verbs) {}

        Tverb_collection_ const& verbs;
        identifier_parser inner;
    };

    template<typename Tverb_collection_>
    struct verb_parser: boost::spirit::qi::primitive_parser<verb_parser<Tverb_collection_> >
    {
        template<typename Tctx_, typename Titer_>
        struct attribute
        {
            typedef node::identifier type;
        };

        template<typename Titer, typename Tctx, typename Tskipper, typename Tattr>
        bool parse(Titer& first, Titer const& last, Tctx& ctx, Tskipper const& skipper, Tattr& attr) const
        {
            const Titer orig(first);
            typename identifier_parser::attribute<Tctx, Titer>::type result;

            if (!inner.parse(first, last, ctx, skipper, result))
            {
                return false;
            }

            BOOST_FOREACH (
                typename boost::range_value<Tverb_collection_>::type const& w,
                verbs)
            {
                if (boost::algorithm::equals(w, result))
                {
                    attr.value = result;
                    return true;
                }
            }

            first = orig;
            return false;
        }

        template<typename Tctx>
        boost::spirit::info what(Tctx const&) const
        {
            return boost::spirit::info("(verb)");
        }

        verb_parser(Tverb_collection_ const& verbs): verbs(verbs) {}

        Tverb_collection_ const& verbs;
        identifier_parser inner;
    };

    struct particle_parser: boost::spirit::qi::primitive_parser<particle_parser>
    {
        template<typename Tctx_, typename Titer_>
        struct attribute
        {
            typedef enum particle_type type;
        };

        template<typename Titer, typename Tctx, typename Tskipper, typename Tattr>
        bool parse(Titer& first, Titer const& last, Tctx& ctx, Tskipper const&, Tattr& attr) const
        {
            const int n(parse_particle(first, last));
            if (n < 0)
                return false;
            if (first != last && is_comma(*first))
                ++first;
            attr = static_cast<enum particle_type>(n);
            return true;
        }

        template<typename Tctx>
        boost::spirit::info what(Tctx const&) const
        {
            return boost::spirit::info("(particle)");
        }
    };

    static bool is_cjk_ideograph(wchar_t c)
    {
        return c >= 0x4e00 && c <= 0x9fcb;
    }

    static bool is_hiragana(wchar_t c)
    {
        return c >= 0x3041 && c <= 0x3096;
    }

    static bool is_no_ident_char(wchar_t c)
    {
        return no_ident_chars.end() != std::find(no_ident_chars.begin(),
                      no_ident_chars.end(), c);
    }

    template<typename Titer>
    static bool reserved_occurs_ahead(Titer const& first, Titer const& last)
    {
        {
            Titer x = first;
            if (parse_particle(x, last) >= 0)
            {
                return true;
            }
        }
        {
            Titer x = first;
            if (parse_conjuction(x, last) >= 0)
            {
                return true;
            }
        }

        return false;
    }

    template<typename Titer>
    static int parse_particle(Titer& first, Titer const& last)
    {
        for (int i = 0; !particles[i].empty(); ++i)
        {
            if (boost::algorithm::starts_with(
                boost::make_iterator_range(first, last), particles[i]))
            {
                first += particles[i].size();
                return i;
            }
        }
        return -1;
    }

    template<typename Titer>
    static int parse_conjuction(Titer& first, Titer const& last)
    {
        for (int i = 0; !conjuctions[i].empty(); ++i)
        {
            if (boost::algorithm::starts_with(
                boost::make_iterator_range(first, last), conjuctions[i]))
            {
                first += conjuctions[i].size();
                return i;
            }
        }
        return -1;
    }

    template<typename Titer>
    static int parse_single_digit(Titer& first, Titer const& last)
    {
        {
            const int n(parse_single_arabic_digit_halfwidth(first, last));
            if (n >= 0)
                return n;
        }
        {
            const int n(parse_single_arabic_digit_fullwidth(first, last));
            if (n >= 0)
                return n;
        }
        {
            const int n(parse_single_kanji_digit(first, last));
            if (n >= 0)
                return n;
        }
        return -1;
    }

    template<typename Titer>
    static int parse_single_arabic_digit(Titer& first, Titer const& last)
    {
        const int n(parse_single_arabic_digit_halfwidth(first, last));
        if (n >= 0)
            return n;
        return parse_single_arabic_digit_fullwidth(first, last);
    }

    template<typename Titer>
    static int parse_single_arabic_digit_fullwidth(Titer& first, Titer const& last)
    {
        const std::wstring::const_iterator r =
            std::find(fullwidth_digits.begin(), fullwidth_digits.end(),
                      *first);
        if (r == fullwidth_digits.end())
            return -1;
        ++first;
        return (r - fullwidth_digits.begin());
    }

    template<typename Titer>
    static int parse_single_arabic_digit_halfwidth(Titer& first, Titer const& last)
    {
        const std::wstring::const_iterator r =
            std::find(halfwidth_digits.begin(), halfwidth_digits.end(),
                      *first);
        if (r == halfwidth_digits.end())
            return -1;
        ++first;
        return (r - halfwidth_digits.begin());
    }

    template<typename Titer>
    static int parse_single_kanji_digit(Titer& first, Titer const& last)
    {
        const std::wstring::const_iterator r(
            std::find(kanji_digits.begin(), kanji_digits.end(), *first));
        if (r == kanji_digits.end())
            return -1;
        ++first;
        return (r - kanji_digits.begin());
    }

    static const std::wstring kanji_digits;
    static const std::wstring halfwidth_digits;
    static const std::wstring fullwidth_digits;
    static const std::wstring particles[];
    static const std::wstring conjuctions[];
    static const std::wstring no_ident_chars;
};

template<typename Tchar_>
struct my_grammar_actions
{
    typedef Tchar_ char_type;

    struct string_appender
    {
        template<typename Tresult_>
        struct appender
        {
            appender(Tresult_& result): result_(result) {}

            template<typename Tchar>
            void operator()(Tchar const& c) const
            {
                append(result_, c);
            }

            Tresult_& result_;
        };

        template<typename Ttarget_, typename Tchar>
        static void append(Ttarget_& target, Tchar const& c, typename boost::disable_if<boost::is_base_of<node::terminal, Ttarget_> >::type* = 0)
        {
            target.push_back(c);
        }

        template<typename Tchar>
        static void append(node::terminal& target, Tchar const& c)
        {
            append(target.value, c);
        }

        template<typename Ttarget, typename Titer>
        static void append(Ttarget& target, Titer const& b, Titer const& e, typename boost::disable_if<boost::is_base_of<node::terminal, Ttarget> >::type* = 0)
        {
            target.append(b, e);
        }

        template<typename Titer>
        static void append(node::terminal& target, Titer const& b, Titer const& e)
        {
            append(target.value, b, e);
        }

        template<typename Tresult>
        static appender<Tresult> make_appender(Tresult& result)
        {
            return appender<Tresult>(result);
        }

        template<typename Tattr, typename Tctx>
        void operator()(Tattr const& a, Tctx const& ctx, boost::spirit::unused_type, typename boost::disable_if<boost::mpl::or_<boost::fusion::traits::is_sequence<Tattr>, boost::is_same<Tattr, char_type> > >::type* = 0) const
        {
            append(boost::fusion::at_c<0>(ctx.attributes), boost::begin(a), boost::end(a));
        }

        template<typename Tattr, typename Tctx>
        void operator()(Tattr const& a, Tctx const& ctx, boost::spirit::unused_type, typename boost::enable_if<boost::is_same<Tattr, char_type> >::type* = 0) const
        {
            append(boost::fusion::at_c<0>(ctx.attributes), a);
        }

        template<typename Tattr, typename Tctx>
        void operator()(Tattr const& a, Tctx const& ctx, boost::spirit::unused_type, typename boost::enable_if<boost::fusion::traits::is_sequence<Tattr> >::type* = 0) const
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

            template<typename Trange>
            void operator()(Trange const& r) const
            {
                string_appender::append(result_, boost::begin(r), boost::end(r));
            }

            Tresult_& result_;
        };

        template<typename Tresult>
        static appender<Tresult> make_appender(Tresult& result)
        {
            return appender<Tresult>(result);
        }

        template<typename Tattr, typename Tctx>
        void operator()(Tattr const& a, Tctx const& ctx, boost::spirit::unused_type) const
        {
            boost::fusion::for_each(a, make_appender(boost::fusion::at_c<0>(ctx.attributes)));
        }
    };

    struct op_builder
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

            template<typename Tchar, typename Tnext_node>
            void operator()(Tchar const& c, Tnext_node const& node, typename boost::enable_if<boost::is_same<Tchar, char_type> >::type* = 0)
            {
                Tchar_ const str[] = { c };
                (*this)(str, node);
            }

            template<typename Tpair>
            void operator()(Tpair const& pair)
            {
                using boost::fusion::at_c;
                (*this)(at_c<0>(pair), at_c<1>(pair));
            }

            Tresult_& next_;
        };

        template<typename Tresult, typename Tnode>
        static appender<Tresult, Tnode> make_appender(Tresult& result, Tnode const& first)
        {
            return appender<Tresult, Tnode>(result, first);
        }

        template<typename Tattr, typename Tctx>
        void operator()(Tattr const& attr, Tctx const& ctx, boost::spirit::unused_type) const
        {
            using boost::fusion::at_c;
            std::for_each(boost::begin(at_c<1>(attr)), boost::end(at_c<1>(attr)), make_appender(at_c<0>(ctx.attributes), at_c<0>(attr)));
        }
    };

    struct sentence_builder
    {
        template<typename Tresult_>
        struct helper
        {
            helper(Tresult_& result): result_(result) {}

            template<typename Tobj_pair>
            void operator()(Tobj_pair const& pair)
            {
                result_.objects.push_back(pair);
            }

            Tresult_& result_;
        };

        template<typename Tresult>
        static helper<Tresult> make_helper(Tresult& result)
        {
            return helper<Tresult>(result);
        }

        template<typename Tattr, typename Tctx>
        void operator()(Tattr const& attr, Tctx const& ctx, boost::spirit::unused_type) const
        {
            using boost::fusion::at_c;
            at_c<0>(ctx.attributes) = node::sentence();
            std::for_each(boost::begin(at_c<0>(attr)), boost::end(at_c<0>(attr)), make_helper(boost::get<node::sentence>(at_c<0>(ctx.attributes))));
            boost::get<node::sentence>(at_c<0>(ctx.attributes)).verb = at_c<1>(attr);
        }
    };
};

const std::wstring my_grammar_parsers::kanji_digits = L"〇一二三四五六七八九";
const std::wstring my_grammar_parsers::halfwidth_digits = L"0123456789";
const std::wstring my_grammar_parsers::fullwidth_digits = L"０１２３４５６７８９";
const std::wstring my_grammar_parsers::particles[] = {
    L"から",
    L"まで",
    L"と",
    L"に",
    L"は",
    L"で",
    L"を",
    L"へ",
    L""
};

const std::wstring my_grammar_parsers::conjuctions[] = {
    L"もし",
    L"ならば",
    L""
};

const std::wstring my_grammar_parsers::no_ident_chars = L" 　\r\n\t「」､、,，｡。.．+＋/／*＊%％&＆−－->＞<＜=＝";

template<typename Titer_, typename Tskipper_>
struct my_grammar
    :  boost::spirit::qi::grammar<Titer_, typename node::statements(), Tskipper_>
{
    typedef my_grammar_actions<typename boost::iterator_value<Titer_>::type> actions;
    typedef typename actions::op_builder op_builder;
    typedef typename actions::sentence_builder sentence_builder;
    typedef typename actions::string_appender string_appender;
 
    my_grammar(): my_grammar::base_type(statements),
                  symbol(verbs), verb(verbs)
    {
        verbs.push_back(L"足");

        using namespace boost::spirit::qi::labels; // for _val and _1
        using boost::spirit::qi::lexeme;
        using boost::spirit::standard_wide::char_;
        using boost::spirit::standard_wide::string;
        using boost::spirit::lit;
        using boost::spirit::omit;
        using boost::spirit::eol;
        using boost::phoenix::at_c;

        eos = omit[eol|char_(L"。.．")];

        statements %= omit[*eos] >> *(statement >> omit[+eos]);

        statement %= assignment | declaration | expr;

        expr %= sentence | greater_less;

        sentence = (*objective >> verb) [sentence_builder()];

        objective %= greater_less >> particle;

        greater_less = (concat >> *((string(L">=")|string(L"＞＝")|string(L"<=")|string(L"＜＝")|string(L">")|string(L"＞")|string(L"<")|string(L"＜")) >> concat)) [op_builder()];

        concat = (add_sub >> *(char_(L"&＆") >> add_sub)) [op_builder()];

        add_sub = (mod >> *(char_(L"+＋−－-") >> mod)) [op_builder()];

        mod = (mul_div >> *(char_(L"%％") >> mul_div)) [op_builder()];

        mul_div = (scalar >> *(char_(L"/／*＊") >> scalar)) [op_builder()];

        scalar %= number | string_literal | symbol;

        string_literal = lexeme[
            (
                L'「' >> *((~char_(L"」\\")) [string_appender()]
                | (char_(L'\\') >> char_) [string_appender()]) >> L'」')
            | (
                L'"' >> *((~char_(L"\"\\")) [string_appender()]
                | (char_(L'\\') >> char_) [string_appender()]) >> L'"')
            | (
                char_(L"“”") >> *((~char_(L"“”\\")) [string_appender()]
                | (char_(L'\\') >> char_) [string_appender()]) >> L'”')
        ];

        // condition %= (lit(L"もし") >> skip_comma) >> expr >> (lit(L"ならば") >> skip_comma);

        assignment %= symbol >> omit[(char_(L'は') >> skip_comma) | char_(L"=＝")] >> expr;

        declaration %= symbol >> omit[lit(L"とは") >> skip_comma] >> symbol;
    }

    std::vector<std::wstring> verbs;

    boost::spirit::qi::rule<Titer_, boost::spirit::unused_type, Tskipper_> eos;
    boost::spirit::qi::rule<Titer_, node::statements(), Tskipper_> statements;
    boost::spirit::qi::rule<Titer_, node::statement(), Tskipper_> statement;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> expr;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> sentence;
    boost::spirit::qi::rule<Titer_, node::objective_pair(), Tskipper_> objective;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> greater_less;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> concat;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> add_sub;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> mod;
    boost::spirit::qi::rule<Titer_, node::expr(), Tskipper_> mul_div;
    boost::spirit::qi::rule<Titer_, node::scalar(), Tskipper_> scalar;
    // boost::spirit::qi::rule<Titer_, node::condition(), Tskipper_> condition;
    boost::spirit::qi::rule<Titer_, node::assignment(), Tskipper_> assignment;
    boost::spirit::qi::rule<Titer_, node::declaration(), Tskipper_> declaration;
    my_grammar_parsers::kanji_number_parser number;
    boost::spirit::qi::rule<Titer_, node::string_literal(), Tskipper_> string_literal;
    my_grammar_parsers::symbol_parser<std::vector<std::wstring> > symbol;
    my_grammar_parsers::verb_parser<std::vector<std::wstring> > verb;
    my_grammar_parsers::particle_parser particle;
    comma_skipper skip_comma;
    boost::spirit::qi::rule<Titer_, std::wstring, Tskipper_> _identifier_remainder;
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
