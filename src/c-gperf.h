/* C code produced by gperf version 2.7.2 */
/* Command-line: gperf -p -j1 -i 1 -g -o -t -G -N is_reserved_word -k'1,3,$'  */
/* Command-line: gperf -p -j1 -i 1 -g -o -t -N is_reserved_word -k1,3,$ c-parse.gperf  */ 
struct resword { char *name; short token; enum rid rid; };

#define TOTAL_KEYWORDS 65
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 16
#define MIN_HASH_VALUE 5
#define MAX_HASH_VALUE 90
/* maximum key range = 86, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static unsigned char asso_values[] =
    {
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91,  1, 91,  6,  3, 20,
       1,  5,  1, 26, 37,  2, 91, 49, 25, 31,
      35, 48, 45, 91, 18, 12,  1, 44, 11, 40,
       2, 91, 39, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
      91, 91, 91, 91, 91, 91
    };
  register int hval = len;

  switch (hval)
    {
      default:
      case 3:
        hval += asso_values[(unsigned char)str[2]];
      case 2:
      case 1:
        hval += asso_values[(unsigned char)str[0]];
        break;
    }
  return hval + asso_values[(unsigned char)str[len - 1]];
}

static struct resword wordlist[] =
  {
    {""}, {""}, {""}, {""}, {""},
    {"if", IF, NORID},
    {""},
    {"int", TYPESPEC, RID_INT},
    {""}, {""},
    {"default", DEFAULT, NORID},
    {"__typeof", TYPEOF, NORID},
    {"__imag__", IMAGPART, NORID},
    {"__typeof__", TYPEOF, NORID},
    {"__inline__", SCSPEC, RID_INLINE},
    {"__asm__", ASM_KEYWORD, NORID},
    {"__inline", SCSPEC, RID_INLINE},
    {"__alignof", ALIGNOF, NORID},
    {"void", TYPESPEC, RID_VOID},
    {"__alignof__", ALIGNOF, NORID},
    {"__extension__", EXTENSION, NORID},
    {"__attribute__", ATTRIBUTE, NORID},
    {"__signed", TYPESPEC, RID_SIGNED},
    {"__attribute", ATTRIBUTE, NORID},
    {"__signed__", TYPESPEC, RID_SIGNED},
    {"__volatile__", TYPE_QUAL, volatile_qualifier},
    {"else", ELSE, NORID},
    {"__volatile", TYPE_QUAL, volatile_qualifier},
    {"__real__", REALPART, NORID},
    {"__const", TYPE_QUAL, const_qualifier},
    {"__restrict", TYPE_QUAL, restrict_qualifier},
    {"__const__", TYPE_QUAL, const_qualifier},
    {"__complex", TYPESPEC, RID_COMPLEX},
    {"__complex__", TYPESPEC, RID_COMPLEX},
    {"assert_type", ASSERT_TYPE, NORID},
    {"__imag", IMAGPART, NORID},
    {"__label__", LABEL, NORID},
    {"struct", STRUCT, NORID},
    {"inline", SCSPEC, RID_INLINE},
    {"restrict", TYPE_QUAL, restrict_qualifier},
    {"for", FOR, NORID},
    {"case", CASE, NORID},
    {"change_type", CHANGE_TYPE, NORID},
    {"__asm", ASM_KEYWORD, NORID},
    {"static", SCSPEC, RID_STATIC},
    {"signed", TYPESPEC, RID_SIGNED},
    {"__builtin_va_arg", VA_ARG, NORID},
    {"extern", SCSPEC, RID_EXTERN},
    {"char", TYPESPEC, RID_CHAR},
    {"volatile", TYPE_QUAL, volatile_qualifier},
    {"__real", REALPART, NORID},
    {"do", DO, NORID},
    {"while", WHILE, NORID},
    {"typeof", TYPEOF, NORID},
    {"typedef", SCSPEC, RID_TYPEDEF},
    {"float", TYPESPEC, RID_FLOAT},
    {"double", TYPESPEC, RID_DOUBLE},
    {"switch", SWITCH, NORID},
    {"sizeof", SIZEOF, NORID},
    {"auto", SCSPEC, RID_AUTO},
    {"return", RETURN, NORID},
    {"const", TYPE_QUAL, const_qualifier},
    {"break", BREAK, NORID},
    {""}, {""},
    {"unsigned", TYPESPEC, RID_UNSIGNED},
    {"short", TYPESPEC, RID_SHORT},
    {"confine", DEEP_RESTRICT, NORID},
    {"continue", CONTINUE, NORID},
    {""},
    {"register", SCSPEC, RID_REGISTER},
    {"asm", ASM_KEYWORD, NORID},
    {""}, {""}, {""}, {""}, {""}, {""}, {""},
    {"goto", GOTO, NORID},
    {""}, {""}, {""}, {""},
    {"enum", ENUM, NORID},
    {""},
    {"union", UNION, NORID},
    {""}, {""}, {""},
    {"long", TYPESPEC, RID_LONG}
  };

#ifdef __GNUC__
__inline
#endif
struct resword *
is_reserved_word (str, len)
     register const char *str;
     register unsigned int len;
{
  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
