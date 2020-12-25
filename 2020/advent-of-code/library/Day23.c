#include <stdio.h>
#include <stdlib.h>
#include <caml/mlvalues.h>

const int N = 1000000;

typedef struct node
{
    int n;
    struct node *next;
} Node;

Node *LUT[N];
int newinput[N];

Node *make(int *arr, int n)
{
    int i;
    Node *start = malloc(sizeof(Node));
    start->n = arr[0];
    LUT[arr[0]] = start;

    Node *prev = start;
    for (i = 1; i < n; i++)
    {
        Node *c = malloc(sizeof(Node));
        c->n = arr[i];
        LUT[arr[i]] = c;
        prev->next = c;
        prev = prev->next;
    }
    prev->next = start;
    return start;
}

void print(Node *s, int n)
{
    int i;
    for (i = 0; i < n; i++)
    {
        printf("%d ", s->n);
        s = s->next;
    }
    printf("\n");
}

int wrap(int v, int s, int e)
{
    if (v >= s && v <= e)
    {
        return v;
    }
    else if (v > s)
    {
        return v % N;
    }
    else
    {
        return N + v;
    }
}

Node *move(Node *current)
{
    // Pluck 3 nodes
    Node *pluck = current->next;
    Node *pluckLast = pluck->next->next;
    int v1 = pluck->n, v2 = pluck->next->n, v3 = pluck->next->next->n;
    current->next = current->next->next->next->next;

    // Find destination
    int destination = wrap(current->n - 1, 1, N);
    while (destination == v1 || destination == v2 || destination == v3)
    {
        destination = wrap(destination - 1, 1, N);
    }

    Node *insertion = LUT[destination];
    pluckLast->next = insertion->next;
    insertion->next = pluck;
    return current->next;
}

void dealloc(Node *n)
{
    // Break the cycle
    Node *start = n->next;
    n->next = NULL;
    while (start != NULL)
    {
        Node *d = start;
        start = start->next;
        free(d);
    }
}

CAMLprim value caml_crabCups()
{
    int input[9] = {1, 2, 3, 4, 8, 7, 5, 9, 6};
    int i;
    for (i = 0; i < 9; i++)
    {
        newinput[i] = input[i];
    }
    for (i = 9; i < N; i++)
    {
        newinput[i] = i + 1;
    }
    Node *n = make(newinput, N);
    int times = 10000000;

    while (times--)
    {
        n = move(n);
    }

    long long answer = (long long)LUT[1]->next->n * (long long)LUT[1]->next->next->n;

    dealloc(n);

    return Val_int(answer);
}
