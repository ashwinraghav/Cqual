$tainted char *getenv(const char *name);
int printf($untainted char *fmt, ...);

char *f3(char *t) { return t; }
char *f2(char *u) { return f3(u); }
char *f1(char *v) { return f2(v); }

int main(void)
{
  char *s, *unclean;

  unclean = getenv("PATH");

  s = f1(unclean);
  printf(s);
}
