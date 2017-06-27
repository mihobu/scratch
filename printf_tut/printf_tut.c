/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                           *
 * printf_tut.c  -  C programming language printf tutorial program           *
 *                                                                           *
 * Written by : Michael H. Burkhardt, 19940413                               *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *                                                                           *
 *     %[<flags>][<field_width>[.<precision>]][<modifiers>]$                 *
 *                                                                           *
 *	``Width or precision or both may be specified as *, in               *
 *	which case the value is computed by converting the next              *
 *	argument(s), which must be int.''                                    *
 *                                        -- The C Programming Language      *
 *                                           by Brian W. Kernighan and       *
 *                                              Dennis M. Ritchie            *
 *                                                                           *
 *       +-------------------++-----------+                                  *
 *       |     FLAGS         || MODIFIERS |                                  *
 *       +---+---+---+---+---++---+---+---+                                  *
 *       | - | + |SPC| # | 0 || l | L | h |                                  *
 * +=====+===+===+===+===+===++===+===+===+                                  *
 * |  d  |   |   |   |XXX|   || @ | @ | @ |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  i  |   |   |   |XXX|   || @ | @ | @ |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  o  |   |   |   | @ |   || @ | @ | @ |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  p  |   |   |   |   |   ||   |   |   |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  u  |   |   |   |XXX|   || @ | @ | @ |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  x  |   |   |   | @ |   || @ | @ | @ |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  X  |   |   |   | @ |   || @ | @ | @ |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  f  |   |   |   | @ |   ||   |   |   |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  e  |   |   |   | @ |   ||   |   |   |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  E  |   |   |   | @ |   ||   |   |   |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  g  |   |   |   | @ |   ||   |   |   |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  G  |   |   |   | @ |   ||   |   |   |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  c  |   |   |   |XXX|   ||   |   |   |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  s  |   |   |   |XXX|   ||   |   |   |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 * |  %  |   |   |   |   |   ||   |   |   |                                  *
 * +-----+---+---+---+---+---++---+---+---+                                  *
 *                                                                           *
 *                                                                           *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <errno.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <time.h>

#define BIGGEST_INT 32767
#define BIGGEST_DBL 128

void intro(struct utsname *sys_info)
{
   int            us_ret;

   us_ret = uname(sys_info);
   if ( us_ret >= 0 ) {
      printf("%s %s\n",sys_info->sysname,sys_info->release);
   }
   else {
      perror("uname");
      exit(us_ret);
   }
}

void choose(char *src,char *dest,char *len_pre)
{
   int    perms;
   int    choice;
   int    i;
   int    x;
   int    y;
   char   y_str[16];

   perms = (int)pow((double)2,(long)strlen(src));
   choice = (int)((long)perms * drand48());
   dest[0] = '\0';
   for ( i = 0; i < strlen(src); i++ ) {
      x = (int)pow((double)2,(double)i);
      if ( ( x & choice ) == x ) {
         y = strlen(dest);
         dest[y] = src[i];
         dest[y+1] = '\0';
      }
   }
   x = (int)((long)10 * drand48());
   y = (int)((long)x * drand48());
   len_pre[0] = '\0';
   if ( x ) {
      sprintf(len_pre,"%d",x);
      if ( y ) {
         strcat(len_pre,".");
         sprintf(y_str,"%d",y);
         strcat(len_pre,y_str);
      }
   }
}

void main()
{
   struct utsname  sys_info;

   int             keepgoing;
   int             num_questions = 0;
   int             num_correct = 0;
   float           perc_correct;
   int             attempt;

   char flags[6];
   char conv_char[32];

   int  x;
   char frmt[256];
   char user_answer[256];
   char correct_answer[256];
   char flags_to_use[32];
   char len_pre[32];
 
   int  val_int;
   double  val_dbl;

   strcpy(flags,"-+ #0");
   strcpy(conv_char,"diouxXfeEgG");

   intro(&sys_info);

   srand48(time(0));

   keepgoing = 1;
   while ( keepgoing ) {
      num_questions++;
      x = (int)((long)strlen(conv_char) * drand48());
      /* printf("\nThe conversion character is `%c'\n",conv_char[x]); */
      switch ( conv_char[x] ) {
         case 'd':
            choose(flags,flags_to_use,len_pre);
            sprintf(frmt,"%%%s%s%c",flags_to_use,len_pre,conv_char[x]);
            val_int = (int)((long)(BIGGEST_INT * 2) * drand48()) - BIGGEST_INT;
            printf("printf(\"%s\",%d);\n",frmt,val_int);
            sprintf(correct_answer,frmt,val_int);
            break;
         case 'i':
            choose(flags,flags_to_use,len_pre);
            sprintf(frmt,"%%%s%s%c",flags_to_use,len_pre,conv_char[x]);
            val_int = (int)((long)(BIGGEST_INT * 2) * drand48()) - BIGGEST_INT;
            printf("printf(\"%s\",%d);\n",frmt,val_int);
            sprintf(correct_answer,frmt,val_int);
            break;
         case 'o':
            choose(flags,flags_to_use,len_pre);
            sprintf(frmt,"%%%s%s%c",flags_to_use,len_pre,conv_char[x]);
            val_int = (int)((long)(BIGGEST_INT) * drand48());
            printf("printf(\"%s\",%d);",frmt,val_int);
            printf("\t%d(dec)=%o(oct)\n",val_int,val_int);
            sprintf(correct_answer,frmt,val_int);
            break;
         case 'u':
            choose(flags,flags_to_use,len_pre);
            sprintf(frmt,"%%%s%s%c",flags_to_use,len_pre,conv_char[x]);
            val_int = (int)((long)(BIGGEST_INT) * drand48());
            printf("printf(\"%s\",%d);\n",frmt,val_int);
            sprintf(correct_answer,frmt,val_int);
            break;
         case 'x':
            choose(flags,flags_to_use,len_pre);
            sprintf(frmt,"%%%s%s%c",flags_to_use,len_pre,conv_char[x]);
            val_int = (int)((long)(BIGGEST_INT) * drand48());
            printf("printf(\"%s\",%d);",frmt,val_int);
            printf("\t%d(dec)=%x(hex)\n",val_int,val_int);
            sprintf(correct_answer,frmt,val_int);
            break;
         case 'X':
            choose(flags,flags_to_use,len_pre);
            sprintf(frmt,"%%%s%s%c",flags_to_use,len_pre,conv_char[x]);
            val_int = (int)((long)(BIGGEST_INT) * drand48());
            printf("printf(\"%s\",%d);",frmt,val_int);
            printf("\t%d(dec)=%x(hex)\n",val_int,val_int);
            sprintf(correct_answer,frmt,val_int);
            break;
         case 'p': break;
         case 'f':
         case 'e':
         case 'E':
         case 'g':
         case 'G':
            choose(flags,flags_to_use,len_pre);
            sprintf(frmt,"%%%s%s%c",flags_to_use,len_pre,conv_char[x]);
            val_dbl = ((long)(BIGGEST_DBL * 2) * drand48()) - BIGGEST_DBL;
            printf("printf(\"%s\",%10.10f);\n",frmt,val_dbl);
            sprintf(correct_answer,frmt,val_dbl);
            break;
         case 's': break;
         default: break;
      }

      for ( attempt = 0; attempt < 2; attempt++ ) {
         printf("                 ....................\n");
         if ( attempt == 0 ) {
            printf("  Enter response:"); fflush(stdout);
         } else {
            printf("Sorry, try again:"); fflush(stdout);
         }
         fgets(user_answer,256,stdin);
         if ( strcmp(user_answer,"quit") == 0 ) {
            keepgoing = 0;
            attempt++;
         }
         else if ( strcmp(user_answer,"help") == 0 ) {
         }
         else {
            if ( strcmp(user_answer,correct_answer) == 0 ) {
               printf("Correct!\n");
               num_correct++;
               attempt++;
            }
            else if ( attempt == 1 ) {
               printf("Sorry. The correct answer is <<%s>>\n",correct_answer);
               printf("             Your answer was <<%s>>\n",user_answer);
            }
         }
      }
      perc_correct = (float)num_correct / (float)num_questions * 100.0;
      printf("\n%d correct out of %d (%.2f%%)\n\n\n",num_correct,num_questions,perc_correct);
   }
}
