#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* identifiers longer than IDENTIFIER_MAX_LENGTH */
/* characters are /allowed/, but truncated.      */
#define IDENTIFIER_MAX_LENGTH 6

/* functions that use more than FUNCTION_MAX_ARGS */
/* arguments cause hard errors                    */
#define FUNCTION_MAX_ARGS 6

/* blocks that have more than BLOCK_MAX_STMTS */
/* statements cause hard errors. note that    */
/* functions bodies count as blocks.          */
#define BLOCK_MAX_STMTS 128

struct token {
  enum {
    ERR_TOK,
    IDENT_TOK,
    NUM_TOK,
    SYMBOL_TOK,
  } type;
  union {
    char IDENT[IDENTIFIER_MAX_LENGTH];
    unsigned int NUM;
    char SYMBOL;
  } as;
};

int lex(struct token *out) {
  int c;
  memset(out, 0, sizeof(*out));
  while (isspace(c = getchar()))
    ;
  if (isalpha(c)) {
    out->type = IDENT_TOK;
    register char *writehead = out->as.IDENT;
    register char *end = (out->as.IDENT) + IDENTIFIER_MAX_LENGTH;
    do
      *writehead++ = c;
    while (writehead < end && isalpha(c = getchar()));
    while (isalpha(c))
      c = getchar();
    ungetc(c, stdin);
    while (writehead < end)
      *writehead++ = '\0';
    return 0;
  } else if (isdigit(c)) {
    out->type = NUM_TOK;
    register unsigned int number = 0;
    do
      number = number * 10 + c - '0';
    while (isdigit(c = getchar()));
    ungetc(c, stdin);
    return 0;
  } else if (c != EOF) {
    out->type = SYMBOL_TOK;
    out->as.SYMBOL = c;
    return 0;
  } else {
    return 1;
  }
}

void print_token(const struct token *tok) {
  switch (tok->type) {
  case ERR_TOK: printf("[error]"); break;
  case IDENT_TOK: printf("[identifier: %-.*s]", sizeof(tok->as.IDENT), tok->as.IDENT); break;
  case NUM_TOK: printf("[number: %u]", tok->as.NUM); break;
  case SYMBOL_TOK: printf("[symbol: `%c`]", tok->as.SYMBOL); break;
  }
  putchar('\n');
}

struct type {
  enum {
    ERR_TYPE,
    INT_TYPE,
    IB_TYPE,
    IP_TYPE,
    ID_TYPE,
    NAT_TYPE,
    NB_TYPE,
    NP_TYPE,
    ND_TYPE
  } type;
};

struct arg {
  struct type type;
  char name[IDENTIFIER_MAX_LENGTH];
};

struct stmt {
  enum { ERR_STMT, FNDECL_STMT, VARDECL_STMT } type;
  union {
    struct {
      struct type type;
      char name[IDENTIFIER_MAX_LENGTH];
      struct arg args[FUNCTION_MAX_ARGS];
      struct stmt (*body)[BLOCK_MAX_STMTS];
    } FNDECL;
    struct {
      struct type type;
      char name[IDENTIFIER_MAX_LENGTH];
    } VARDECL;
  } as;
};

static const char fn_keyword[IDENTIFIER_MAX_LENGTH] = "fn";
static const char var_keyword[IDENTIFIER_MAX_LENGTH] = "var";
static const char int_keyword[IDENTIFIER_MAX_LENGTH] = "int";
static const char ib_keyword[IDENTIFIER_MAX_LENGTH] = "ib";
static const char ip_keyword[IDENTIFIER_MAX_LENGTH] = "ip";
static const char id_keyword[IDENTIFIER_MAX_LENGTH] = "id";
static const char nat_keyword[IDENTIFIER_MAX_LENGTH] = "nat";
static const char nb_keyword[IDENTIFIER_MAX_LENGTH] = "nb";
static const char np_keyword[IDENTIFIER_MAX_LENGTH] = "np";
static const char nd_keyword[IDENTIFIER_MAX_LENGTH] = "nd";

int type_parse(struct type *out) {
  static struct type dummy;
  struct token tok;

  if (!out)
    out = &dummy;
  memset(out, 0, sizeof(*out));

  if (lex(&tok) || tok.type != IDENT_TOK) return 1;
  if (!memcmp(tok.as.IDENT, &int_keyword, IDENTIFIER_MAX_LENGTH)) { out->type = INT_TYPE; }
  else if (!memcmp(tok.as.IDENT, &ib_keyword, IDENTIFIER_MAX_LENGTH)) { out->type = IB_TYPE; }
  else if (!memcmp(tok.as.IDENT, &ip_keyword, IDENTIFIER_MAX_LENGTH)) { out->type = IP_TYPE; }
  else if (!memcmp(tok.as.IDENT, &id_keyword, IDENTIFIER_MAX_LENGTH)) { out->type = ID_TYPE; }
  else if (!memcmp(tok.as.IDENT, &nat_keyword, IDENTIFIER_MAX_LENGTH)) { out->type = NAT_TYPE; }
  else if (!memcmp(tok.as.IDENT, &nb_keyword, IDENTIFIER_MAX_LENGTH)) { out->type = NB_TYPE; }
  else if (!memcmp(tok.as.IDENT, &np_keyword, IDENTIFIER_MAX_LENGTH)) { out->type = NP_TYPE; }
  else if (!memcmp(tok.as.IDENT, &nd_keyword, IDENTIFIER_MAX_LENGTH)) { out->type = ND_TYPE; }
  else { out->type = ERR_TYPE; return 1; }

  return 0;
}

int is_symbol(const struct token *tok, char sym) {
  return tok->type == SYMBOL_TOK && tok->as.SYMBOL == sym;
}

int is_ident(const struct token *tok, const char (*ident)[IDENTIFIER_MAX_LENGTH]) {
  return tok->type == IDENT_TOK && !memcmp(&tok->as.IDENT, ident, IDENTIFIER_MAX_LENGTH);
}

void cp_ident(char (*str)[IDENTIFIER_MAX_LENGTH], const struct token *tok) {
  if (tok->type == IDENT_TOK)
    memcpy(str, &tok->as.IDENT, IDENTIFIER_MAX_LENGTH);
  else
    memset(str, 0, IDENTIFIER_MAX_LENGTH);
}

#define TRY(...) if(!(__VA_ARGS__));else return 1
#define ASSERT_SYMBOL(tokp, sym) if((lex(tokp)) && is_symbol((tokp), sym));else return 1

static int arglist_parse(struct arg (*out)[FUNCTION_MAX_ARGS]) {
  struct token tok;
  int argi = 0;

  ASSERT_SYMBOL(&tok, '(');

  TRY(lex(&tok));
  while (!is_symbol(&tok, ')')) {
    if (argi) {
      if (!is_symbol(&tok, ',')) return 1;
      TRY(lex(&tok));
    }

    if (tok.type != IDENT_TOK) return 1;
    if (argi < FUNCTION_MAX_ARGS)
      cp_ident(&(*out)[argi].name, &tok);

    ASSERT_SYMBOL(&tok, ':');

    if (argi < FUNCTION_MAX_ARGS)
      TRY(type_parse(&(*out)[argi].type));
    else
      TRY(type_parse((void *) 0)); /* ignore argument */
    ++argi;

    TRY(lex(&tok));
  }

  return 0;
}

int parse(struct stmt *out) {
  struct token tok;
  memset(out, 0, sizeof(*out));

  TRY(lex(&tok));

  while (is_symbol(&tok, '#')) {
    do
      TRY(lex(&tok));
    while (!is_symbol(&tok, '#'));
    TRY(lex(&tok));
  }

  /* fn NAME(ARGS: ARGTYPE): RETTYPE; */
  /* ^^                               */
  if (is_ident(&tok, &fn_keyword)) {
    out->type = FNDECL_STMT;

    /* fn NAME(ARGS: ARGTYPE): RETTYPE; */
    /*    ^^^^                          */
    TRY(lex(&tok));
    if (tok.type != IDENT_TOK) return 1;
    cp_ident(&out->as.FNDECL.name, &tok);

    /* fn NAME(ARGS: ARGTYPE): RETTYPE; */
    /*        ^^^^^^^^^^^^^^^           */
    arglist_parse(&out->as.FNDECL.args);

    /* fn NAME(ARGS: ARGTYPE): RETTYPE; */
    /*                       ^          */
    TRY(lex(&tok));
    if (!is_symbol(&tok, ':')) return 1;

    /* fn NAME(ARGS: ARGTYPE): RETTYPE; */
    /*                         ^^^^^^^  */
    TRY(type_parse(&out->as.FNDECL.type));

    /* fn NAME(ARGS: ARGTYPE): RETTYPE; */
    /*                                ^ */
    TRY(lex(&tok));
    if (!is_symbol(&tok, ';')) return 1;
  } else {
    out->type = ERR_STMT;
  }
  return 0;
}

int main(void) {
  struct stmt stmt;
  while (!parse(&stmt)) {
    switch (stmt.type) {
    case FNDECL_STMT:
      printf("[fndecl rettype=%d name=\"%.*s\" args=(");
      for (int argi = 0; stmt.as.FNDECL.args[argi].type.type; ++argi) {
        if (argi) printf(", ");
        printf("%.*s: %d", IDENTIFIER_MAX_LENGTH, stmt.as.FNDECL.args[argi].name, stmt.as.FNDECL.args[argi].type.type);
      }
      printf(")]\n");
      break;
    case ERR_STMT:
      printf("[error?]\n");
      break;
    }
  }
  return 0;
}
