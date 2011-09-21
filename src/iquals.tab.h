#ifndef BISON_IQUALS_TAB_H
# define BISON_IQUALS_TAB_H

#ifndef YYSTYPE
typedef union {
  char *str;
  qual qual;
} yystype;
# define YYSTYPE yystype
# define YYSTYPE_IS_TRIVIAL 1
#endif
# define	CONSTANT	257
# define	VARIABLE	258
# define	EQ	259
# define	IMPLIES	260
# define	LEQ	261
# define	UNIFY	262
# define	ERROR	263


extern YYSTYPE yylval;

#endif /* not BISON_IQUALS_TAB_H */
