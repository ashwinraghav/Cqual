#ifndef BISON_LATTICE_PARSE_TAB_H
# define BISON_LATTICE_PARSE_TAB_H

#ifndef YYSTYPE
typedef union {
  char *str;
} yystype;
# define YYSTYPE yystype
# define YYSTYPE_IS_TRIVIAL 1
#endif
# define	QUALIFIER	257
# define	STRING	258
# define	COLOR	259
# define	FLOW_SENSITIVE	260
# define	EQ	261
# define	LEVEL	262
# define	NEG	263
# define	NONPROP	264
# define	ORDER	265
# define	PARTIAL	266
# define	POS	267
# define	REF	268
# define	SIGN	269
# define	VALUE	270


extern YYSTYPE lyylval;

#endif /* not BISON_LATTICE_PARSE_TAB_H */
