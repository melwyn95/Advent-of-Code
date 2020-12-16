#include <stdio.h>
#include <caml/mlvalues.h>

CAMLprim value caml_find30000000thTerm()
{
    static int lookup[30000000][2] = {0};
    int initial[6] = {8, 13, 1, 0, 18, 9};
    int s = 7, e = 30000000;
    int prev_tup[2], next, prev = initial[5];
    int i, a;

    for (i = 0; i < 6; i++)
    {
        lookup[initial[i]][0] = i + 1;
        lookup[initial[i]][1] = -1;
        prev_tup[0] = i + 1;
        prev_tup[1] = -1;
    }

    for (i = s; i <= e; i++)
    {
        if (prev_tup[1] == -1)
        {
            next = 0;
        }
        else
        {
            next = prev_tup[0] - prev_tup[1];
        }

        if (lookup[next][0] == 0 && lookup[next][1] == 0)
        {
            prev_tup[0] = i;
            prev_tup[1] = -1;
            lookup[next][0] = i;
            lookup[next][1] = -1;
        }
        else
        {
            a = lookup[next][0];
            prev_tup[0] = i;
            prev_tup[1] = a;
            lookup[next][0] = i;
            lookup[next][1] = a;
        }
        prev = next;
    }
    return Val_int(prev);
}
