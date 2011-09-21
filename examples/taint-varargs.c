int sprintf($_1_2 char *, $untainted char *, $_2 ...);

int main(void)
{
  $tainted char *s;
  char buf[100];

  sprintf(buf, "%s", s);
}
