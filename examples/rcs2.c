$$a _op_deref($$a *$NONYEAR);

   int printf(const char * $NONYEAR format, $NONYEAR ...);
   int sprintf(char * str, const char * format, $NONYEAR ...);

   char * $YYYY date2str(char * $RCSYEAR date, char * $NONYEAR datebuf) {
     char *p = (char * $NONYEAR)date;
     while (*p++ != '.')
       ;
     sprintf(datebuf,
             "19%.*s/%.2s/%.2s" + (((char * $NONYEAR)date)[2]=='.' ?  0  :  2),
             (int)(p-date-1), date, p, p+3
             );
     return (char * $YYYY)datebuf;
   }
   int main(void) {
     char *today = (char * $RCSYEAR)"99.05.12";
     char *nextyear = (char * $RCSYEAR)"2000.05.12";
     char *datebuf = "\0         ";
     printf("today is %s\n", date2str(today,datebuf));
     printf("nextyear is %s\n", date2str(nextyear,datebuf));
     return 1;
   }
