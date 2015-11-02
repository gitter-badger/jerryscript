/* Copyright 2015 University of Szeged.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "js-parser-defines.h"

/* Strict mode string literal in directive prologues */
#define PARSER_USE_STRICT_LITERAL  "use strict"
#define PARSER_USE_STRICT_LENGTH   10

/**
 * Parser statement types.
 *
 * When a new statement is added, the following
 * functions may need to be updated as well:
 *
 *  - parser_statement_length()
 *  - parser_parse_break_statement()
 *  - parser_parse_continue_statement()
 *  - parser_free_jumps()
 *  - 'case LEXER_RIGHT_BRACE:' in parser_parse_statements()
 *  - 'if (context_p->token.type == LEXER_RIGHT_BRACE)' in parser_parse_statements()
 *  - 'switch (context_p->stack_top_uint8)' in parser_parse_statements()
 */
typedef enum
{
  PARSER_STATEMENT_START,
  PARSER_STATEMENT_BLOCK,
  PARSER_STATEMENT_LABEL,
  PARSER_STATEMENT_IF,
  PARSER_STATEMENT_ELSE,
  /* From switch -> for-in : break target statements */
  PARSER_STATEMENT_SWITCH,
  PARSER_STATEMENT_SWITCH_NO_DEFAULT,
  /* From do-while -> for->in : continue target statements */
  PARSER_STATEMENT_DO_WHILE,
  PARSER_STATEMENT_WHILE,
  PARSER_STATEMENT_FOR,
  /* From for->in -> try : instructions with context
   * Break and continue uses another instruction form
   * when crosses their borders. */
  PARSER_STATEMENT_FOR_IN,
  PARSER_STATEMENT_WITH,
  PARSER_STATEMENT_TRY,
} parser_statement_type;

/**
 * Loop statement.
 */
typedef struct
{
  parser_branch_item *branch_list_p;       /**< list of breaks and continues targeting this statement */
} parser_loop_statement;

/**
 * Label statement.
 */
typedef struct
{
  lexer_literal_location label_ident;     /**< name of the label */
  parser_branch_item *break_list_p;       /**< list of breaks targeting this label */
} parser_label_statement;

/**
 * If/else statement.
 */
typedef struct
{
  parser_branch branch;                   /**< branch to the end */
} parser_if_else_statement;

/**
 * Switch statement.
 */
typedef struct
{
  parser_branch default_branch;           /**< branch to the default case */
  parser_branch_item *branch_list_p;      /**< branches of case statements */
} parser_switch_statement;

/**
 * Do-while statement.
 */
typedef struct
{
  uint32_t start_offset;                  /**< start byte code offset */
} parser_do_while_statement;

/**
 * While statement.
 */
typedef struct
{
  parser_branch branch;                   /**< branch to the end */
  lexer_range condition_range;            /**< condition part */
  uint32_t start_offset;                  /**< start byte code offset */
} parser_while_statement;

/**
 * For statement.
 */
typedef struct
{
  parser_branch branch;                   /**< branch to the end */
  lexer_range condition_range;            /**< condition part */
  lexer_range expression_range;           /**< increase part */
  uint32_t start_offset;                  /**< start byte code offset */
} parser_for_statement;

/**
 * For-in statement.
 */
typedef struct
{
  parser_branch branch;                   /**< branch to the end */
  uint32_t start_offset;                  /**< start byte code offset */
} parser_for_in_statement;

/**
 * With statement.
 */
typedef struct
{
  parser_branch branch;                   /**< branch to the end */
} parser_with_statement;

/**
 * Lexer token types.
 */
typedef enum
{
  parser_try_block,                       /**< try block */
  parser_catch_block,                     /**< catch block */
  parser_finally_block,                   /**< finally block */
} parser_try_block_type;

/**
 * Try statement.
 */
typedef struct
{
  parser_try_block_type type;             /**< current block type */
  parser_branch branch;                   /**< branch to the end of the current block */
} parser_try_statement;

/**
 * Returns the data consumed by a statement. It can be used
 * to skip undesired frames on the stack during frame search.
 *
 * @return size consumed by a statement.
 */
static size_t PARSER_INLINE
parser_statement_length (uint8_t type)
{
  static const uint8_t statement_lengths[12] =
  {
    /* PARSER_STATEMENT_BLOCK */
    1,
    /* PARSER_STATEMENT_LABEL */
    sizeof (parser_label_statement) + 1,
    /* PARSER_STATEMENT_IF */
    sizeof (parser_if_else_statement) + 1,
    /* PARSER_STATEMENT_ELSE */
    sizeof (parser_if_else_statement) + 1,
    /* PARSER_STATEMENT_SWITCH */
    sizeof (parser_switch_statement) + sizeof (parser_loop_statement) + 1,
    /* PARSER_STATEMENT_SWITCH_NO_DEFAULT */
    sizeof (parser_switch_statement) + sizeof (parser_loop_statement) + 1,
    /* PARSER_STATEMENT_DO_WHILE */
    sizeof (parser_do_while_statement) + sizeof (parser_loop_statement) + 1,
    /* PARSER_STATEMENT_WHILE */
    sizeof (parser_while_statement) + sizeof (parser_loop_statement) + 1,
    /* PARSER_STATEMENT_FOR */
    sizeof (parser_for_statement) + sizeof (parser_loop_statement) + 1,
    /* PARSER_STATEMENT_FOR_IN */
    sizeof (parser_for_in_statement) + sizeof (parser_loop_statement) + 1,
    /* PARSER_STATEMENT_WITH */
    sizeof (parser_with_statement) + 1,
    /* PARSER_STATEMENT_TRY */
    sizeof (parser_try_statement) + 1,
  };

  PARSER_ASSERT (type >= PARSER_STATEMENT_BLOCK && type <= PARSER_STATEMENT_TRY);
  PARSER_ASSERT (PARSER_STATEMENT_TRY - PARSER_STATEMENT_BLOCK == 11);

  return statement_lengths[type - PARSER_STATEMENT_BLOCK];
}

/**
 * Initialize a range from the current location.
 */
static void PARSER_INLINE
parser_save_range (parser_context *context_p, /**< context */
                   lexer_range *range_p, /**< destination range */
                   const uint8_t *source_end_p) /**< source end */
{
  range_p->source_p = context_p->source_p;
  range_p->source_end_p = source_end_p;
  range_p->line = context_p->line;
  range_p->column = context_p->column;
} /* parser_save_range */

/**
 * Set the current location on the stack.
 */
static void PARSER_INLINE
parser_set_range (parser_context *context_p, /**< context */
                  lexer_range *range_p) /**< destination range */
{
  context_p->source_p = range_p->source_p;
  context_p->source_end_p = range_p->source_end_p;
  context_p->line = range_p->line;
  context_p->column = range_p->column;
} /* parser_set_range */

/**
 * Initialize stack iterator.
 */
static void PARSER_INLINE
parser_stack_iterator_init (parser_context *context_p, /**< context */
                            parser_stack_iterator *iterator) /**< iterator */
{
  iterator->current_p = context_p->stack.first_p;
  iterator->current_position = context_p->stack.last_position;
} /* parser_stack_iterator_init */

/**
 * Read the next byte from the stack.
 */
static uint8_t PARSER_INLINE
parser_stack_iterator_read_uint8 (parser_stack_iterator *iterator) /**< iterator */
{
  PARSER_ASSERT (iterator->current_position > 0 && iterator->current_position <= PARSER_STACK_PAGE_SIZE);
  return iterator->current_p->bytes[iterator->current_position - 1];
} /* parser_stack_iterator_read_uint8 */

/**
 * Change last byte of the stack.
 */
static void PARSER_INLINE
parser_stack_change_last_uint8 (parser_context *context_p, /**< context */
                                uint8_t new_value) /**< new value */
{
  parser_mem_page *page_p = context_p->stack.first_p;

  PARSER_ASSERT (page_p != NULL
                 && context_p->stack_top_uint8 == page_p->bytes[context_p->stack.last_position - 1]);

  page_p->bytes[context_p->stack.last_position - 1] = new_value;
  context_p->stack_top_uint8 = new_value;
} /* parser_stack_iterator_read_uint8 */

/**
 * Scan for a token on the stack.
 */
static void PARSER_INLINE
parser_scan_until (parser_context *context_p, /**< context */
                   lexer_range *range_p, /**< destination range */
                   lexer_token_type type1, /**< terminator token type */
                   lexer_token_type type2) /**< terminator token type */
{
  size_t bracket_depth = 0;
  size_t conditional_depth = 0;

  parser_save_range (context_p, range_p, context_p->source_p);

  lexer_next_token (context_p);

  while (bracket_depth + conditional_depth > 0
         || (context_p->token.type != type1 && context_p->token.type != type2))
  {
    if (context_p->token.type == LEXER_EOS)
    {
      parser_raise_error (context_p, "Unexpected end of stream.");
    }

    if (LEXER_IS_LEFT_BRACKET (context_p->token.type))
    {
      bracket_depth++;
    }

    if (bracket_depth > 0
        && LEXER_IS_RIGHT_BRACKET (context_p->token.type))
    {
      bracket_depth--;
    }

    if (bracket_depth == 0
        && context_p->token.type == LEXER_QUESTION_MARK)
    {
      conditional_depth++;
    }

    if (bracket_depth == 0
        && conditional_depth > 0
        && context_p->token.type == LEXER_COLON)
    {
      conditional_depth--;
    }

    range_p->source_end_p = context_p->source_p;
    lexer_next_token (context_p);
  }
}

/**
 * Parse var statement.
 */
static void
parser_parse_var_statement (parser_context *context_p) /**< context */
{
  PARSER_ASSERT (context_p->token.type == LEXER_KEYW_VAR);

  while (PARSER_TRUE)
  {
    lexer_expect_identifier (context_p, LEXER_IDENT_LITERAL);
    PARSER_ASSERT (context_p->token.type == LEXER_LITERAL
                   && context_p->token.literal_location.type == LEXER_IDENT_LITERAL);

    context_p->literal_object.literal_p->status_flags |= LEXER_FLAG_VAR;

    parser_emit_cbc_literal_from_token (context_p, CBC_PUSH_IDENT);

    lexer_next_token (context_p);

    if (context_p->token.type == LEXER_ASSIGN)
    {
      parser_parse_expression (context_p,
                               PARSE_EXPR_STATEMENT | PARSE_EXPR_NO_COMMA | PARSE_EXPR_HAS_LITERAL);
    }
    else
    {
      PARSER_ASSERT (context_p->last_cbc_opcode == CBC_PUSH_IDENT);
      /* We don't need to assign anything to this variable. */
      context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
    }

    if (context_p->token.type != LEXER_COMMA)
    {
      break;
    }
  }
} /* parser_parse_var_statement */

/**
 * Parse function statement.
 */
static void
parser_parse_function_statement (parser_context *context_p) /**< context */
{
  PARSER_ASSERT (context_p->token.type == LEXER_KEYW_FUNCTION);

  lexer_expect_identifier (context_p, LEXER_IDENT_LITERAL);
  PARSER_ASSERT (context_p->token.type == LEXER_LITERAL
                 && context_p->token.literal_location.type == LEXER_IDENT_LITERAL);

  context_p->literal_object.literal_p->status_flags |= LEXER_FLAG_VAR | LEXER_FLAG_FUNC_TARGET;

  lexer_construct_function_object (context_p,
                                   context_p->literal_object.index,
                                   PARSER_IS_FUNCTION | PARSER_IS_CLOSURE);
  lexer_next_token (context_p);
} /* parser_parse_function_statement */

/**
 * Parse if statement (starting part).
 */
static void
parser_parse_if_statement_start (parser_context *context_p) /**< context */
{
  parser_if_else_statement if_statement;

  lexer_next_token (context_p);

  if (context_p->token.type != LEXER_LEFT_PAREN)
  {
    parser_raise_error (context_p, "Expected '(' after if.");
  }

  parser_parse_expression (context_p, PARSE_EXPR);
  parser_emit_cbc_forward_branch (context_p,
                                  CBC_BRANCH_IF_FALSE_FORWARD,
                                  &if_statement.branch);

  parser_stack_push (context_p, &if_statement, sizeof (parser_if_else_statement));
  parser_stack_push_uint8 (context_p, PARSER_STATEMENT_IF);
  parser_stack_iterator_init (context_p, &context_p->last_statement);
} /* parser_parse_if_statement_start */

/**
 * Parse if statement (ending part).
 */
static int
parser_parse_if_statement_end (parser_context *context_p) /**< context */
{
  parser_if_else_statement if_statement;
  parser_if_else_statement else_statement;
  parser_stack_iterator iterator;

  PARSER_ASSERT (context_p->stack_top_uint8 == PARSER_STATEMENT_IF);

  if (context_p->token.type != LEXER_KEYW_ELSE)
  {
    parser_stack_pop_uint8 (context_p);
    parser_stack_pop (context_p, &if_statement, sizeof (parser_if_else_statement));
    parser_stack_iterator_init (context_p, &context_p->last_statement);

    parser_set_branch_to_current_position (context_p, &if_statement.branch);

    return PARSER_FALSE;
  }

  parser_stack_change_last_uint8 (context_p, PARSER_STATEMENT_ELSE);
  parser_stack_iterator_init (context_p, &iterator);
  parser_stack_iterator_skip (&iterator, 1);
  parser_stack_iterator_read (&iterator, &if_statement, sizeof (parser_if_else_statement));

  parser_emit_cbc_forward_branch (context_p,
                                  CBC_JUMP_FORWARD,
                                  &else_statement.branch);

  parser_set_branch_to_current_position (context_p, &if_statement.branch);

  parser_stack_iterator_write (&iterator, &else_statement, sizeof (parser_if_else_statement));

  lexer_next_token (context_p);
  return PARSER_TRUE;
} /* parser_parse_if_statement_end */

/**
 * Parse with statement (starting part).
 */
static void
parser_parse_with_statement_start (parser_context *context_p) /**< context */
{
  parser_with_statement with_statement;

  if (context_p->status_flags & PARSER_IS_STRICT)
  {
    parser_raise_error (context_p, "With statement is not allowed in strict mode.");
  }

  lexer_next_token (context_p);

  if (context_p->token.type != LEXER_LEFT_PAREN)
  {
    parser_raise_error (context_p, "Expected '(' after with.");
  }

  parser_parse_expression (context_p, PARSE_EXPR);
  parser_emit_cbc_ext_forward_branch (context_p,
                                      CBC_EXT_WITH_CREATE_CONTEXT,
                                      &with_statement.branch);

  parser_stack_push (context_p, &with_statement, sizeof (parser_with_statement));
  parser_stack_push_uint8 (context_p, PARSER_STATEMENT_WITH);
  parser_stack_iterator_init (context_p, &context_p->last_statement);
} /* parser_parse_with_statement_start */

/**
 * Parse do-while statement (ending part).
 */
static void
parser_parse_do_while_statement_end (parser_context *context_p) /**< context */
{
  parser_do_while_statement do_while_statement;
  parser_loop_statement loop;

  PARSER_ASSERT (context_p->stack_top_uint8 == PARSER_STATEMENT_DO_WHILE);

  if (context_p->token.type != LEXER_KEYW_WHILE)
  {
    parser_raise_error (context_p, "While expected after do-while loop.");
  }

  lexer_next_token (context_p);
  if (context_p->token.type != LEXER_LEFT_PAREN)
  {
    parser_raise_error (context_p, "Expected '(' after while.");
  }

  parser_stack_pop_uint8 (context_p);
  parser_stack_pop (context_p, &loop, sizeof (parser_loop_statement));
  parser_stack_pop (context_p, &do_while_statement, sizeof (parser_do_while_statement));
  parser_stack_iterator_init (context_p, &context_p->last_statement);

  parser_set_continues_to_current_position (context_p, loop.branch_list_p);
  parser_parse_expression (context_p, PARSE_EXPR);

  if (context_p->last_cbc_opcode != CBC_PUSH_FALSE)
  {
    cbc_opcode opcode = CBC_BRANCH_IF_TRUE_BACKWARD;
    if (context_p->last_cbc_opcode == CBC_LOGICAL_NOT)
    {
      context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
      opcode = CBC_BRANCH_IF_FALSE_BACKWARD;
    }
    else if (context_p->last_cbc_opcode == CBC_PUSH_TRUE)
    {
      context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
      opcode = CBC_JUMP_BACKWARD;
    }
    parser_emit_cbc_backward_branch (context_p, opcode, do_while_statement.start_offset);
  }
  else
  {
    context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
  }

  parser_set_breaks_to_current_position (context_p, loop.branch_list_p);
} /* parser_parse_do_while_statement */

/**
 * Parse while statement (starting part).
 */
static void
parser_parse_while_statement_start (parser_context *context_p) /**< context */
{
  parser_while_statement while_statement;
  parser_loop_statement loop;

  PARSER_ASSERT (context_p->token.type == LEXER_KEYW_WHILE);
  lexer_next_token (context_p);

  if (context_p->token.type != LEXER_LEFT_PAREN)
  {
    parser_raise_error (context_p, "Expected '(' after while.");
  }

  parser_emit_cbc_forward_branch (context_p, CBC_JUMP_FORWARD, &while_statement.branch);

  PARSER_ASSERT (context_p->last_cbc_opcode == PARSER_CBC_UNAVAILABLE);
  while_statement.start_offset = context_p->byte_code_size;

  /* The conditional part is processed at the end. */
  parser_scan_until (context_p, &while_statement.condition_range, LEXER_RIGHT_PAREN, LEXER_RIGHT_PAREN);
  lexer_next_token (context_p);

  loop.branch_list_p = NULL;

  parser_stack_push (context_p, &while_statement, sizeof (parser_while_statement));
  parser_stack_push (context_p, &loop, sizeof (parser_loop_statement));
  parser_stack_push_uint8 (context_p, PARSER_STATEMENT_WHILE);
  parser_stack_iterator_init (context_p, &context_p->last_statement);
} /* parser_parse_while_statement_start */

/**
 * Parse while statement (ending part).
 */
static void PARSER_NOINLINE
parser_parse_while_statement_end (parser_context *context_p) /**< context */
{
  parser_while_statement while_statement;
  parser_loop_statement loop;
  lexer_token current_token;
  lexer_range range;
  cbc_opcode opcode;

  PARSER_ASSERT (context_p->stack_top_uint8 == PARSER_STATEMENT_WHILE);

  parser_stack_pop_uint8 (context_p);
  parser_stack_pop (context_p, &loop, sizeof (parser_loop_statement));
  parser_stack_pop (context_p, &while_statement, sizeof (parser_while_statement));
  parser_stack_iterator_init (context_p, &context_p->last_statement);

  parser_save_range (context_p, &range, context_p->source_end_p);
  current_token = context_p->token;

  parser_set_branch_to_current_position (context_p, &while_statement.branch);
  parser_set_continues_to_current_position (context_p, loop.branch_list_p);

  parser_set_range (context_p, &while_statement.condition_range);
  lexer_next_token (context_p);

  parser_parse_expression (context_p, PARSE_EXPR);
  if (context_p->token.type != LEXER_EOS)
  {
    parser_raise_error (context_p, "Invalid expression.");
  }

  opcode = CBC_BRANCH_IF_TRUE_BACKWARD;
  if (context_p->last_cbc_opcode == CBC_LOGICAL_NOT)
  {
    context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
    opcode = CBC_BRANCH_IF_FALSE_BACKWARD;
  }
  else if (context_p->last_cbc_opcode == CBC_PUSH_TRUE)
  {
    context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
    opcode = CBC_JUMP_BACKWARD;
  }

  parser_emit_cbc_backward_branch (context_p, opcode, while_statement.start_offset);
  parser_set_breaks_to_current_position (context_p, loop.branch_list_p);

  parser_set_range (context_p, &range);
  context_p->token = current_token;
} /* parser_parse_while_statement_end */

/**
 * Parse for statement (starting part).
 */
static void
parser_parse_for_statement_start (parser_context *context_p) /**< context */
{
  parser_loop_statement loop;
  lexer_range start_range;

  PARSER_ASSERT (context_p->token.type == LEXER_KEYW_FOR);
  lexer_next_token (context_p);

  if (context_p->token.type != LEXER_LEFT_PAREN)
  {
    parser_raise_error (context_p, "Expected '(' after for.");
  }

  parser_scan_until (context_p, &start_range, LEXER_SEMICOLON, LEXER_KEYW_IN);

  if (context_p->token.type == LEXER_KEYW_IN)
  {
    parser_for_in_statement for_in_statement;
    lexer_range range;

    lexer_next_token (context_p);
    parser_parse_expression (context_p, PARSE_EXPR);

    if (context_p->token.type != LEXER_RIGHT_PAREN)
    {
      parser_raise_error (context_p, "Expected ')' token.");
    }

    parser_emit_cbc_ext_forward_branch (context_p,
                                        CBC_EXT_FOR_IN_CREATE_CONTEXT,
                                        &for_in_statement.branch);

    PARSER_ASSERT (context_p->last_cbc_opcode == PARSER_CBC_UNAVAILABLE);
    for_in_statement.start_offset = context_p->byte_code_size;

    parser_save_range (context_p, &range, context_p->source_end_p);
    parser_set_range (context_p, &start_range);
    lexer_next_token (context_p);

    if (context_p->token.type == LEXER_KEYW_VAR)
    {
      uint16_t literal_index;

      lexer_expect_identifier (context_p, LEXER_IDENT_LITERAL);
      PARSER_ASSERT (context_p->token.type == LEXER_LITERAL
                     && context_p->token.literal_location.type == LEXER_IDENT_LITERAL);

      context_p->literal_object.literal_p->status_flags |= LEXER_FLAG_VAR;

      literal_index = context_p->literal_object.index;

      lexer_next_token (context_p);

      if (context_p->token.type == LEXER_ASSIGN)
      {
        parser_branch branch;

        /* Initialiser is never executed. */
        parser_emit_cbc_forward_branch (context_p, CBC_JUMP_FORWARD, &branch);
        lexer_next_token (context_p);
        parser_parse_expression (context_p,
                                 PARSE_EXPR_STATEMENT | PARSE_EXPR_NO_COMMA);
        parser_set_branch_to_current_position (context_p, &branch);
      }

      parser_emit_cbc_ext (context_p, CBC_EXT_FOR_IN_GET_NEXT);
      parser_emit_cbc_literal (context_p, CBC_ASSIGN_IDENT, literal_index);
    }
    else
    {
      cbc_argument argument;
      uint16_t opcode;

      parser_parse_expression (context_p, PARSE_EXPR);

      argument = context_p->last_cbc;
      opcode = context_p->last_cbc_opcode;

      if (opcode == CBC_PUSH_IDENT)
      {
        opcode = CBC_ASSIGN_IDENT;
        context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
      }
      else if (opcode == CBC_PROP_GET)
      {
        opcode = CBC_ASSIGN;
        context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
      }
      else if (opcode == CBC_PROP_STRING_GET)
      {
        opcode = CBC_ASSIGN_PROP_STRING;
        context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
      }
      else
      {
        /* A runtime error will happen. */
        parser_emit_cbc_ext (context_p, CBC_EXT_PUSH_UNDEFINED_BASE);
        opcode = CBC_ASSIGN;
      }

      parser_emit_cbc_ext (context_p, CBC_EXT_FOR_IN_GET_NEXT);
      parser_flush_cbc (context_p);

      context_p->last_cbc = argument;
      context_p->last_cbc_opcode = opcode;
    }

    if (context_p->token.type != LEXER_EOS)
    {
      parser_raise_error (context_p, "Expected in keyword.");
    }

    parser_flush_cbc (context_p);
    parser_set_range (context_p, &range);
    lexer_next_token (context_p);

    loop.branch_list_p = NULL;

    parser_stack_push (context_p, &for_in_statement, sizeof (parser_for_in_statement));
    parser_stack_push (context_p, &loop, sizeof (parser_loop_statement));
    parser_stack_push_uint8 (context_p, PARSER_STATEMENT_FOR_IN);
    parser_stack_iterator_init (context_p, &context_p->last_statement);
  }
  else
  {
    parser_for_statement for_statement;

    start_range.source_end_p = context_p->source_end_p;
    parser_set_range (context_p, &start_range);
    lexer_next_token (context_p);

    if (context_p->token.type != LEXER_SEMICOLON)
    {
      if (context_p->token.type == LEXER_KEYW_VAR)
      {
        parser_parse_var_statement (context_p);
      }
      else
      {
        parser_parse_expression (context_p, PARSE_EXPR_STATEMENT);
      }

      if (context_p->token.type != LEXER_SEMICOLON)
      {
        parser_raise_error (context_p, "Expected ';' token.");
      }
    }

    parser_emit_cbc_forward_branch (context_p, CBC_JUMP_FORWARD, &for_statement.branch);

    PARSER_ASSERT (context_p->last_cbc_opcode == PARSER_CBC_UNAVAILABLE);
    for_statement.start_offset = context_p->byte_code_size;

    /* The conditional and expression parts are processed at the end. */
    parser_scan_until (context_p, &for_statement.condition_range, LEXER_SEMICOLON, LEXER_SEMICOLON);
    parser_scan_until (context_p, &for_statement.expression_range, LEXER_RIGHT_PAREN, LEXER_RIGHT_PAREN);
    lexer_next_token (context_p);

    loop.branch_list_p = NULL;

    parser_stack_push (context_p, &for_statement, sizeof (parser_for_statement));
    parser_stack_push (context_p, &loop, sizeof (parser_loop_statement));
    parser_stack_push_uint8 (context_p, PARSER_STATEMENT_FOR);
    parser_stack_iterator_init (context_p, &context_p->last_statement);
  }
} /* parser_parse_for_statement_start */

/**
 * Parse for statement (ending part).
 */
static void PARSER_NOINLINE
parser_parse_for_statement_end (parser_context *context_p) /**< context */
{
  parser_for_statement for_statement;
  parser_loop_statement loop;
  lexer_token current_token;
  lexer_range range;
  cbc_opcode opcode;

  PARSER_ASSERT (context_p->stack_top_uint8 == PARSER_STATEMENT_FOR);

  parser_stack_pop_uint8 (context_p);
  parser_stack_pop (context_p, &loop, sizeof (parser_loop_statement));
  parser_stack_pop (context_p, &for_statement, sizeof (parser_for_statement));
  parser_stack_iterator_init (context_p, &context_p->last_statement);

  parser_save_range (context_p, &range, context_p->source_end_p);
  current_token = context_p->token;

  parser_set_range (context_p, &for_statement.expression_range);
  lexer_next_token (context_p);

  parser_set_continues_to_current_position (context_p, loop.branch_list_p);

  if (context_p->token.type != LEXER_EOS)
  {
    parser_parse_expression (context_p, PARSE_EXPR_STATEMENT);

    if (context_p->token.type != LEXER_EOS)
    {
      parser_raise_error (context_p, "Invalid expression.");
    }
  }

  parser_set_branch_to_current_position (context_p, &for_statement.branch);

  parser_set_range (context_p, &for_statement.condition_range);
  lexer_next_token (context_p);

  if (context_p->token.type != LEXER_EOS)
  {
    parser_parse_expression (context_p, PARSE_EXPR);

    if (context_p->token.type != LEXER_EOS)
    {
      parser_raise_error (context_p, "Invalid expression.");
    }

    opcode = CBC_BRANCH_IF_TRUE_BACKWARD;
    if (context_p->last_cbc_opcode == CBC_LOGICAL_NOT)
    {
      context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
      opcode = CBC_BRANCH_IF_FALSE_BACKWARD;
    }
    else if (context_p->last_cbc_opcode == CBC_PUSH_TRUE)
    {
      context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
      opcode = CBC_JUMP_BACKWARD;
    }
  }
  else
  {
    opcode = CBC_JUMP_BACKWARD;
  }

  parser_emit_cbc_backward_branch (context_p, opcode, for_statement.start_offset);
  parser_set_breaks_to_current_position (context_p, loop.branch_list_p);

  parser_set_range (context_p, &range);
  context_p->token = current_token;
} /* parser_parse_for_statement_end */

/**
 * Parse switch statement (starting part).
 */
static void PARSER_NOINLINE
parser_parse_switch_statement_start (parser_context *context_p) /**< context */
{
  parser_switch_statement switch_statement;
  parser_loop_statement loop;
  parser_stack_iterator iterator;
  lexer_range switch_body_start;
  size_t depth;
  int switch_case_was_found;
  int default_case_was_found;
  parser_branch_item *cases_p = NULL;

  PARSER_ASSERT (context_p->token.type == LEXER_KEYW_SWITCH);
  lexer_next_token (context_p);

  if (context_p->token.type != LEXER_LEFT_PAREN)
  {
    parser_raise_error (context_p, "Expected '(' after while.");
  }

  parser_parse_expression (context_p, PARSE_EXPR);

  if (context_p->token.type != LEXER_LEFT_BRACE)
  {
    parser_raise_error (context_p, "Switch body must start with '{'.");
  }

  parser_save_range (context_p, &switch_body_start, context_p->source_end_p);
  lexer_next_token (context_p);

  if (context_p->token.type == LEXER_RIGHT_BRACE)
  {
    /* Unlikely case, but possible. */
    parser_emit_cbc (context_p, CBC_POP);
    parser_flush_cbc (context_p);
    parser_stack_push_uint8 (context_p, PARSER_STATEMENT_BLOCK);
    parser_stack_iterator_init (context_p, &context_p->last_statement);
    return;
  }

  if (context_p->token.type != LEXER_KEYW_CASE
      && context_p->token.type != LEXER_KEYW_DEFAULT)
  {
    parser_raise_error (context_p, "Invalid switch body.");
  }

  /* The reason of using an iterator is error management. If an error
   * occures, parser_free_jumps() free all data. However, the branches
   * created by parser_emit_cbc_forward_branch_item() would not be freed.
   * To free these branches, the current switch data is always stored
   * on the stack. If any change happens, this data is updated. Updates
   * are done using the iterator. */

  switch_statement.branch_list_p = NULL;
  loop.branch_list_p = NULL;

  parser_stack_push (context_p, &switch_statement, sizeof (parser_switch_statement));
  parser_stack_iterator_init (context_p, &iterator);
  parser_stack_push (context_p, &loop, sizeof (parser_loop_statement));
  parser_stack_push_uint8 (context_p, PARSER_STATEMENT_SWITCH);
  parser_stack_iterator_init (context_p, &context_p->last_statement);

  depth = 0;
  switch_case_was_found = PARSER_FALSE;
  default_case_was_found = PARSER_FALSE;

  while (PARSER_TRUE)
  {
    if (context_p->token.type == LEXER_EOS)
    {
      parser_raise_error (context_p, "Unexpected end of switch statement.");
    }

    if (LEXER_IS_LEFT_BRACKET (context_p->token.type))
    {
      depth++;
    }
    else if (depth == 0)
    {
      if (context_p->token.type == LEXER_KEYW_DEFAULT)
      {
        if (default_case_was_found)
        {
          parser_raise_error (context_p, "Multiple default case is not allowed.");
        }

        lexer_next_token (context_p);
        if (context_p->token.type != LEXER_COLON)
        {
          parser_raise_error (context_p, "Expected ':' after default case.");
        }

        default_case_was_found = PARSER_TRUE;
      }

      if (context_p->token.type == LEXER_KEYW_CASE
          || context_p->token.type == LEXER_RIGHT_BRACE)
      {
        if (switch_case_was_found)
        {
          parser_branch_item *new_case_p;

          if (context_p->token.type == LEXER_KEYW_CASE)
          {
            parser_emit_cbc (context_p, CBC_SWITCH_STRICT_EQUAL);
          }
          else
          {
            /* We don't duplicate the value for the last case. */
            parser_emit_cbc (context_p, CBC_STRICT_EQUAL);
          }
          new_case_p = parser_emit_cbc_forward_branch_item (context_p,
                                                            CBC_BRANCH_IF_TRUE_FORWARD,
                                                            NULL);
          if (cases_p == NULL)
          {
            switch_statement.branch_list_p = new_case_p;
            parser_stack_iterator_write (&iterator, &switch_statement, sizeof (parser_switch_statement));
          }
          else
          {
            cases_p->next_p = new_case_p;
          }
          cases_p = new_case_p;
        }

        /* End of switch statement. */
        if (context_p->token.type == LEXER_RIGHT_BRACE)
        {
          break;
        }

        lexer_next_token (context_p);

        parser_parse_expression (context_p, PARSE_EXPR);

        if (context_p->token.type != LEXER_COLON)
        {
          parser_raise_error (context_p, "Expected ':' after switch case.");
        }
        switch_case_was_found = PARSER_TRUE;
      }

      if (context_p->token.type == LEXER_DOT)
      {
        lexer_next_token (context_p);
      }
    }

    if (LEXER_IS_RIGHT_BRACKET (context_p->token.type))
    {
      if (depth == 0)
      {
        parser_raise_error (context_p, "Unexpected closing bracket.");
      }
      depth--;
    }

    lexer_next_token (context_p);
  }

  PARSER_ASSERT (switch_case_was_found || default_case_was_found);

  if (!switch_case_was_found)
  {
    /* There was no case statement, so the expression result
     * of the switch must be popped from the stack */
    parser_emit_cbc (context_p, CBC_POP);
  }

  parser_emit_cbc_forward_branch (context_p, CBC_JUMP_FORWARD, &switch_statement.default_branch);
  parser_stack_iterator_write (&iterator, &switch_statement, sizeof (parser_switch_statement));

  if (!default_case_was_found)
  {
    parser_stack_change_last_uint8 (context_p, PARSER_STATEMENT_SWITCH_NO_DEFAULT);
  }

  parser_set_range (context_p, &switch_body_start);
  lexer_next_token (context_p);
} /* parser_parse_switch_statement_start */

/**
 * Parse try statement.
 */
static void
parser_parse_try_statement (parser_context *context_p) /**< context */
{
  parser_try_statement try_statement;
  parser_stack_iterator iterator;

  PARSER_ASSERT (context_p->stack_top_uint8 == PARSER_STATEMENT_TRY);

  parser_stack_iterator_init (context_p, &iterator);
  parser_stack_iterator_skip (&iterator, 1);
  parser_stack_iterator_read (&iterator, &try_statement, sizeof (parser_try_statement));

  lexer_next_token (context_p);

  if (try_statement.type == parser_finally_block)
  {
    parser_emit_cbc (context_p, CBC_CONTEXT_END);
    parser_set_branch_to_current_position (context_p, &try_statement.branch);
  }
  else
  {
    parser_set_branch_to_current_position (context_p, &try_statement.branch);

    if (try_statement.type == parser_catch_block)
    {
      if (context_p->token.type != LEXER_KEYW_FINALLY)
      {
        parser_emit_cbc (context_p, CBC_CONTEXT_END);
        parser_flush_cbc (context_p);
        try_statement.type = parser_finally_block;
      }
    }
    else if (try_statement.type == parser_try_block)
    {
      if (context_p->token.type != LEXER_KEYW_CATCH
          && context_p->token.type != LEXER_KEYW_FINALLY)
      {
        parser_raise_error (context_p, "Catch or finally expected.");
      }
    }
  }

  if (try_statement.type == parser_finally_block)
  {
    parser_stack_pop (context_p, NULL, sizeof (parser_try_statement) + 1);
    parser_stack_iterator_init (context_p, &context_p->last_statement);
    return;
  }

  if (context_p->token.type == LEXER_KEYW_CATCH)
  {
    uint16_t literal_index;

    lexer_next_token (context_p);

    if (context_p->token.type != LEXER_LEFT_PAREN)
    {
      parser_raise_error (context_p, "Expected '(' after catch.");
    }

    lexer_expect_identifier (context_p, LEXER_IDENT_LITERAL);
    PARSER_ASSERT (context_p->token.type == LEXER_LITERAL
                   && context_p->token.literal_location.type == LEXER_IDENT_LITERAL);

    literal_index = context_p->literal_object.index;

    lexer_next_token (context_p);

    if (context_p->token.type != LEXER_RIGHT_PAREN)
    {
      parser_raise_error (context_p, "Expected ')' after catch identifier.");
    }

    lexer_next_token (context_p);

    if (context_p->token.type != LEXER_LEFT_BRACE)
    {
      parser_raise_error (context_p, "Expected '{' after catch identifier.");
    }


    try_statement.type = parser_catch_block;
    parser_emit_cbc_ext_forward_branch (context_p,
                                        CBC_EXT_CATCH,
                                        &try_statement.branch);

    parser_emit_cbc_literal (context_p, CBC_ASSIGN_IDENT, literal_index);
    parser_flush_cbc (context_p);
  }
  else
  {
    PARSER_ASSERT (context_p->token.type == LEXER_KEYW_FINALLY);

    lexer_next_token (context_p);

    if (context_p->token.type != LEXER_LEFT_BRACE)
    {
      parser_raise_error (context_p, "Expected '{' after finally.");
    }

    try_statement.type = parser_finally_block;
    parser_emit_cbc_ext_forward_branch (context_p,
                                        CBC_EXT_FINALLY,
                                        &try_statement.branch);
  }

  lexer_next_token (context_p);
  parser_stack_iterator_write (&iterator, &try_statement, sizeof (parser_try_statement));
} /* parser_parse_try_statement */

/**
 * Parse default statement.
 */
static void
parser_parse_default_statement (parser_context *context_p) /**< context */
{
  parser_stack_iterator iterator;
  parser_switch_statement switch_statement;

  if (context_p->stack_top_uint8 != PARSER_STATEMENT_SWITCH
      && context_p->stack_top_uint8 != PARSER_STATEMENT_SWITCH_NO_DEFAULT)
  {
    parser_raise_error (context_p, "Default statement must be part of a switch statement.");
  }

  lexer_next_token (context_p);
  /* Already checked in parser_parse_switch_statement_start. */
  PARSER_ASSERT (context_p->token.type == LEXER_COLON);
  lexer_next_token (context_p);

  parser_stack_iterator_init (context_p, &iterator);
  parser_stack_iterator_skip (&iterator, 1 + sizeof (parser_loop_statement));
  parser_stack_iterator_read (&iterator, &switch_statement, sizeof (parser_switch_statement));

  parser_set_branch_to_current_position (context_p, &switch_statement.default_branch);
} /* parser_parse_default_statement */

/**
 * Parse case statement.
 */
static void
parser_parse_case_statement (parser_context *context_p) /**< context */
{
  parser_stack_iterator iterator;
  parser_switch_statement switch_statement;
  lexer_range dummy_range;
  parser_branch_item *branch_p;

  if (context_p->stack_top_uint8 != PARSER_STATEMENT_SWITCH
      && context_p->stack_top_uint8 != PARSER_STATEMENT_SWITCH_NO_DEFAULT)
  {
    parser_raise_error (context_p, "Case statement must be part of a switch statement.");
  }

  parser_scan_until (context_p, &dummy_range, LEXER_COLON, LEXER_COLON);
  lexer_next_token (context_p);

  parser_stack_iterator_init (context_p, &iterator);
  parser_stack_iterator_skip (&iterator, 1 + sizeof (parser_loop_statement));
  parser_stack_iterator_read (&iterator, &switch_statement, sizeof (parser_switch_statement));

  /* Free memory after the case statement is found. */

  branch_p = switch_statement.branch_list_p;
  PARSER_ASSERT (branch_p != NULL);
  switch_statement.branch_list_p = branch_p->next_p;
  parser_stack_iterator_write (&iterator, &switch_statement, sizeof (parser_switch_statement));

  parser_set_branch_to_current_position (context_p, &branch_p->branch);
  parser_free (branch_p);
} /* parser_parse_case_statement */

/**
 * Parse break statement.
 */
static void
parser_parse_break_statement (parser_context *context_p) /**< context */
{
  parser_stack_iterator iterator;
  cbc_opcode opcode = CBC_JUMP_FORWARD;

  lexer_next_token (context_p);
  parser_stack_iterator_init (context_p, &iterator);

  if (!context_p->token.was_newline
      && context_p->token.type == LEXER_LITERAL
      && context_p->token.literal_location.type == LEXER_IDENT_LITERAL)
  {
    /* The label with the same name is searched on the stack. */
    while (PARSER_TRUE)
    {
      uint8_t type = parser_stack_iterator_read_uint8 (&iterator);
      if (type == PARSER_STATEMENT_START)
      {
        parser_raise_error (context_p, "Target label of a break not found.");
      }

      if (type == PARSER_STATEMENT_FOR_IN
          || type == PARSER_STATEMENT_WITH
          || type == PARSER_STATEMENT_TRY)
      {
        opcode = CBC_JUMP_FORWARD_EXIT_CONTEXT;
      }

      if (type == PARSER_STATEMENT_LABEL)
      {
        parser_label_statement label_statement;

        parser_stack_iterator_skip (&iterator, 1);
        parser_stack_iterator_read (&iterator, &label_statement, sizeof (parser_label_statement));

        if (lexer_same_identifiers (&context_p->token.literal_location, &label_statement.label_ident))
        {
          label_statement.break_list_p = parser_emit_cbc_forward_branch_item (context_p,
                                                                              opcode,
                                                                              label_statement.break_list_p);
          parser_stack_iterator_write (&iterator, &label_statement, sizeof (parser_label_statement));
          lexer_next_token (context_p);
          return;
        }
        parser_stack_iterator_skip (&iterator, sizeof (parser_label_statement));
      }
      else
      {
        parser_stack_iterator_skip (&iterator, parser_statement_length (type));
      }
    }
  }

  /* The first switch or loop statement is searched. */
  while (PARSER_TRUE)
  {
    uint8_t type = parser_stack_iterator_read_uint8 (&iterator);
    if (type == PARSER_STATEMENT_START)
    {
      parser_raise_error (context_p, "Break without label must be inside a loop or switch.");
    }

    if (type == PARSER_STATEMENT_FOR_IN
        || type == PARSER_STATEMENT_WITH
        || type == PARSER_STATEMENT_TRY)
    {
      opcode = CBC_JUMP_FORWARD_EXIT_CONTEXT;
    }

    if (type == PARSER_STATEMENT_SWITCH
        || type == PARSER_STATEMENT_SWITCH_NO_DEFAULT
        || type == PARSER_STATEMENT_DO_WHILE
        || type == PARSER_STATEMENT_WHILE
        || type == PARSER_STATEMENT_FOR
        || type == PARSER_STATEMENT_FOR_IN)
    {
      parser_loop_statement loop;

      parser_stack_iterator_skip (&iterator, 1);
      parser_stack_iterator_read (&iterator, &loop, sizeof (parser_loop_statement));
      loop.branch_list_p = parser_emit_cbc_forward_branch_item (context_p,
                                                                opcode,
                                                                loop.branch_list_p);
      parser_stack_iterator_write (&iterator, &loop, sizeof (parser_loop_statement));
      return;
    }

    parser_stack_iterator_skip (&iterator, parser_statement_length (type));
  }
} /* parser_parse_break_statement */

/**
 * Parse continue statement.
 */
static void
parser_parse_continue_statement (parser_context *context_p) /**< context */
{
  parser_stack_iterator iterator;
  cbc_opcode opcode = CBC_JUMP_FORWARD;

  lexer_next_token (context_p);
  parser_stack_iterator_init (context_p, &iterator);

  if (!context_p->token.was_newline
      && context_p->token.type == LEXER_LITERAL
      && context_p->token.literal_location.type == LEXER_IDENT_LITERAL)
  {
    parser_stack_iterator loop_iterator;
    int for_in_was_seen = PARSER_FALSE;

    loop_iterator.current_p = NULL;

    /* The label with the same name is searched on the stack. */
    while (PARSER_TRUE)
    {
      uint8_t type = parser_stack_iterator_read_uint8 (&iterator);

      if (type == PARSER_STATEMENT_START)
      {
        parser_raise_error (context_p, "Target label of a continue not found.");
      }

      /* Only those labels are checked, whose are label of a loop. */
      if (loop_iterator.current_p != NULL && type == PARSER_STATEMENT_LABEL)
      {
        parser_label_statement label_statement;

        parser_stack_iterator_skip (&iterator, 1);
        parser_stack_iterator_read (&iterator, &label_statement, sizeof (parser_label_statement));

        if (lexer_same_identifiers (&context_p->token.literal_location, &label_statement.label_ident))
        {
          parser_loop_statement loop;

          parser_stack_iterator_skip (&loop_iterator, 1);
          parser_stack_iterator_read (&loop_iterator, &loop, sizeof (parser_loop_statement));
          loop.branch_list_p = parser_emit_cbc_forward_branch_item (context_p,
                                                                    opcode,
                                                                    loop.branch_list_p);
          loop.branch_list_p->branch.offset |= CBC_HIGHEST_BIT_MASK;
          parser_stack_iterator_write (&loop_iterator, &loop, sizeof (parser_loop_statement));
          lexer_next_token (context_p);
          return;
        }
        parser_stack_iterator_skip (&iterator, sizeof (parser_label_statement));
        continue;
      }

      if (type == PARSER_STATEMENT_WITH
          || type == PARSER_STATEMENT_TRY
          || for_in_was_seen)
      {
        opcode = CBC_JUMP_FORWARD_EXIT_CONTEXT;
      }
      else if (type == PARSER_STATEMENT_FOR_IN)
      {
        for_in_was_seen = PARSER_TRUE;
      }

      if (type == PARSER_STATEMENT_DO_WHILE
          || type == PARSER_STATEMENT_WHILE
          || type == PARSER_STATEMENT_FOR
          || type == PARSER_STATEMENT_FOR_IN)
      {
        loop_iterator = iterator;
      }
      else
      {
        loop_iterator.current_p = NULL;
      }

      parser_stack_iterator_skip (&iterator, parser_statement_length (type));
    }
  }

  /* The first loop statement is searched. */
  while (PARSER_TRUE)
  {
    uint8_t type = parser_stack_iterator_read_uint8 (&iterator);
    if (type == PARSER_STATEMENT_START)
    {
      parser_raise_error (context_p, "Continue must be inside a loop.");
    }

    if (type == PARSER_STATEMENT_DO_WHILE
        || type == PARSER_STATEMENT_WHILE
        || type == PARSER_STATEMENT_FOR
        || type == PARSER_STATEMENT_FOR_IN)
    {
      parser_loop_statement loop;

      parser_stack_iterator_skip (&iterator, 1);
      parser_stack_iterator_read (&iterator, &loop, sizeof (parser_loop_statement));
      loop.branch_list_p = parser_emit_cbc_forward_branch_item (context_p,
                                                                opcode,
                                                                loop.branch_list_p);
      loop.branch_list_p->branch.offset |= CBC_HIGHEST_BIT_MASK;
      parser_stack_iterator_write (&iterator, &loop, sizeof (parser_loop_statement));
      return;
    }

    if (type == PARSER_STATEMENT_WITH
        || type == PARSER_STATEMENT_TRY)
    {
      opcode = CBC_JUMP_FORWARD_EXIT_CONTEXT;
    }

    parser_stack_iterator_skip (&iterator, parser_statement_length (type));
  }
} /* parser_parse_continue_statement */

/**
 * Parse label statement.
 */
static void
parser_parse_label (parser_context *context_p, /**< context */
                    lexer_literal_location *label_literal_p) /**< saved literal */
{
  parser_stack_iterator iterator;
  parser_label_statement label_statement;

  parser_stack_iterator_init (context_p, &iterator);

  while (PARSER_TRUE)
  {
    uint8_t type = parser_stack_iterator_read_uint8 (&iterator);
    if (type == PARSER_STATEMENT_START)
    {
      break;
    }

    if (type == PARSER_STATEMENT_LABEL)
    {
      parser_stack_iterator_skip (&iterator, 1);
      parser_stack_iterator_read (&iterator, &label_statement, sizeof (parser_label_statement));
      parser_stack_iterator_skip (&iterator, sizeof (parser_label_statement));

      if (lexer_same_identifiers (label_literal_p, &label_statement.label_ident))
      {
        parser_raise_error (context_p, "Duplicated label.");
      }
    }
    else
    {
      parser_stack_iterator_skip (&iterator, parser_statement_length (type));
    }
  }

  label_statement.label_ident = *label_literal_p;
  label_statement.break_list_p = NULL;
  parser_stack_push (context_p, &label_statement, sizeof (parser_label_statement));
  parser_stack_push_uint8 (context_p, PARSER_STATEMENT_LABEL);
  parser_stack_iterator_init (context_p, &context_p->last_statement);
} /* parser_parse_label */

/**
 * Parse statements.
 */
void
parser_parse_statements (parser_context *context_p) /**< context */
{
  /* Statement parsing cannot be nested. */
  PARSER_ASSERT (context_p->last_statement.current_p == NULL);
  parser_stack_push_uint8 (context_p, PARSER_STATEMENT_START);
  parser_stack_iterator_init (context_p, &context_p->last_statement);

  while (context_p->token.type == LEXER_LITERAL
         && context_p->token.literal_location.type == LEXER_STRING_LITERAL)
  {
    lexer_literal_location literal_location;

    PARSER_ASSERT (context_p->stack_depth == 0);

    literal_location = context_p->token.literal_location;
    lexer_next_token (context_p);

    if (context_p->token.type != LEXER_SEMICOLON
        && context_p->token.type != LEXER_RIGHT_BRACE)
    {
      if (!context_p->token.was_newline
          || LEXER_IS_BINARY_OP_TOKEN (context_p->token.type)
          || context_p->token.type == LEXER_LEFT_PAREN
          || context_p->token.type == LEXER_LEFT_SQUARE
          || context_p->token.type == LEXER_DOT)
      {
        lexer_construct_literal_object (context_p, &literal_location, LEXER_STRING_LITERAL);
        parser_emit_cbc_literal_from_token (context_p, CBC_PUSH_LITERAL);
        /* The literal_is_reserved is reused for saving the token. */
        context_p->token.literal_is_reserved = context_p->token.type;
        context_p->token.type = LEXER_EXPRESSION_START;
        break;
      }
    }

    if (literal_location.length == PARSER_USE_STRICT_LENGTH
        && !literal_location.has_escape
        && memcmp (PARSER_USE_STRICT_LITERAL, literal_location.char_p, PARSER_USE_STRICT_LENGTH) == 0)
    {
      context_p->status_flags |= PARSER_IS_STRICT;

      if (context_p->token.type == LEXER_LITERAL
          && context_p->token.literal_location.type == LEXER_IDENT_LITERAL
          && context_p->token.literal_is_reserved)
      {
        parser_raise_error (context_p, "Identifier name is reserved in strict mode.");
      }

#ifdef PARSER_DEBUG
      if (context_p->is_show_opcodes)
      {
        printf ("  Note: switch to strict mode\n\n");
      }
#endif /* PARSER_DEBUG */
    }

    if (context_p->token.type == LEXER_SEMICOLON)
    {
      lexer_next_token (context_p);
    }
  }

  if (context_p->status_flags & PARSER_IS_STRICT
      && context_p->status_flags & PARSER_HAS_NON_STRICT_ARG)
  {
    parser_raise_error (context_p, "Non strict argument definition.");
  }

  while (context_p->token.type != LEXER_EOS
         || context_p->stack_top_uint8 != PARSER_STATEMENT_START)
  {
    int statement_terminator_required;

    PARSER_ASSERT (context_p->stack_depth == 0);

    switch (context_p->token.type)
    {
      case LEXER_SEMICOLON:
      {
        break;
      }

      case LEXER_RIGHT_BRACE:
      {
        if (context_p->stack_top_uint8 == PARSER_STATEMENT_LABEL
            || context_p->stack_top_uint8 == PARSER_STATEMENT_IF
            || context_p->stack_top_uint8 == PARSER_STATEMENT_ELSE
            || context_p->stack_top_uint8 == PARSER_STATEMENT_DO_WHILE
            || context_p->stack_top_uint8 == PARSER_STATEMENT_WHILE
            || context_p->stack_top_uint8 == PARSER_STATEMENT_FOR
            || context_p->stack_top_uint8 == PARSER_STATEMENT_FOR_IN
            || context_p->stack_top_uint8 == PARSER_STATEMENT_WITH)
        {
          parser_raise_error (context_p, "Statement expected.");
        }
        break;
      }

      case LEXER_LEFT_BRACE:
      {
        parser_stack_push_uint8 (context_p, PARSER_STATEMENT_BLOCK);
        parser_stack_iterator_init (context_p, &context_p->last_statement);
        lexer_next_token (context_p);
        continue;
        /* FALLTHRU */
      }

      case LEXER_KEYW_VAR:
      {
        parser_parse_var_statement (context_p);
        break;
      }

      case LEXER_KEYW_FUNCTION:
      {
        parser_parse_function_statement (context_p);
        continue;
        /* FALLTHRU */
      }

      case LEXER_KEYW_IF:
      {
        parser_parse_if_statement_start (context_p);
        continue;
        /* FALLTHRU */
      }

      case LEXER_KEYW_SWITCH:
      {
        parser_parse_switch_statement_start (context_p);
        continue;
        /* FALLTHRU */
      }

      case LEXER_KEYW_DO:
      {
        parser_do_while_statement do_while_statement;
        parser_loop_statement loop;

        PARSER_ASSERT (context_p->last_cbc_opcode == PARSER_CBC_UNAVAILABLE);

        do_while_statement.start_offset = context_p->byte_code_size;
        loop.branch_list_p = NULL;

        parser_stack_push (context_p, &do_while_statement, sizeof (parser_do_while_statement));
        parser_stack_push (context_p, &loop, sizeof (parser_loop_statement));
        parser_stack_push_uint8 (context_p, PARSER_STATEMENT_DO_WHILE);
        parser_stack_iterator_init (context_p, &context_p->last_statement);
        lexer_next_token (context_p);
        continue;
        /* FALLTHRU */
      }

      case LEXER_KEYW_WHILE:
      {
        parser_parse_while_statement_start (context_p);
        continue;
        /* FALLTHRU */
      }

      case LEXER_KEYW_FOR:
      {
        parser_parse_for_statement_start (context_p);
        continue;
        /* FALLTHRU */
      }

      case LEXER_KEYW_WITH:
      {
        parser_parse_with_statement_start (context_p);
        continue;
        /* FALLTHRU */
      }

      case LEXER_KEYW_TRY:
      {
        parser_try_statement try_statement;

        lexer_next_token (context_p);

        if (context_p->token.type != LEXER_LEFT_BRACE)
        {
          parser_raise_error (context_p, "Expected '{' after try keyword.");
        }

        try_statement.type = parser_try_block;
        parser_emit_cbc_ext_forward_branch (context_p,
                                            CBC_EXT_TRY_CREATE_CONTEXT,
                                            &try_statement.branch);

        parser_stack_push (context_p, &try_statement, sizeof (parser_try_statement));
        parser_stack_push_uint8 (context_p, PARSER_STATEMENT_TRY);
        parser_stack_iterator_init (context_p, &context_p->last_statement);
        lexer_next_token (context_p);
        continue;
        /* FALLTHRU */
      }

      case LEXER_KEYW_DEFAULT:
      {
        parser_parse_default_statement (context_p);
        continue;
        /* FALLTHRU */
      }

      case LEXER_KEYW_CASE:
      {
        parser_parse_case_statement (context_p);
        continue;
        /* FALLTHRU */
      }

      case LEXER_KEYW_BREAK:
      {
        parser_parse_break_statement (context_p);
        break;
      }

      case LEXER_KEYW_CONTINUE:
      {
        parser_parse_continue_statement (context_p);
        break;
      }

      case LEXER_KEYW_THROW:
      {
        lexer_next_token (context_p);
        if (context_p->token.was_newline)
        {
          parser_raise_error (context_p, "Primary expression expected after throw.");
        }
        parser_parse_expression (context_p, PARSE_EXPR);
        parser_emit_cbc_ext (context_p, CBC_EXT_THROW);
        break;
      }

      case LEXER_KEYW_RETURN:
      {
        if (!(context_p->status_flags & PARSER_IS_FUNCTION))
        {
          parser_raise_error (context_p, "Return statement is not within a function.");
        }

        lexer_next_token (context_p);
        if (context_p->token.was_newline
            || context_p->token.type == LEXER_SEMICOLON
            || context_p->token.type == LEXER_RIGHT_BRACE)
        {
          parser_emit_cbc (context_p, CBC_RETURN_WITH_UNDEFINED);
          break;
        }

        parser_parse_expression (context_p, PARSE_EXPR);
        parser_emit_cbc (context_p, CBC_RETURN);
        break;
      }

      case LEXER_KEYW_DEBUGGER:
      {
        parser_emit_cbc_ext (context_p, CBC_EXT_DEBUGGER);
        lexer_next_token (context_p);
        break;
      }

      case LEXER_LITERAL:
      {
        if (context_p->token.literal_location.type == LEXER_IDENT_LITERAL)
        {
          lexer_literal_location literal_location = context_p->token.literal_location;

          lexer_next_token (context_p);

          if (context_p->token.type == LEXER_COLON)
          {
            parser_parse_label (context_p, &literal_location);
            lexer_next_token (context_p);
            continue;
          }

          lexer_construct_literal_object (context_p, &literal_location, LEXER_IDENT_LITERAL);
          parser_emit_cbc_literal_from_token (context_p, CBC_PUSH_IDENT);
          /* The literal_is_reserved is reused for saving the token. */
          context_p->token.literal_is_reserved = context_p->token.type;
          context_p->token.type = LEXER_EXPRESSION_START;
        }
        /* FALLTHRU */
      }

      default:
      {
        int options = PARSE_EXPR_BLOCK;

        if (context_p->status_flags & PARSER_IS_FUNCTION)
        {
          options = PARSE_EXPR_STATEMENT;
        }

        if (context_p->token.type == LEXER_EXPRESSION_START)
        {
          /* The literal_is_reserved is reused for saving the token. */
          context_p->token.type = context_p->token.literal_is_reserved;
          options |= PARSE_EXPR_HAS_LITERAL;
        }

        parser_parse_expression (context_p, options);
        break;
      }
    }

    parser_flush_cbc (context_p);

    statement_terminator_required = PARSER_TRUE;
    while (1)
    {
      if (statement_terminator_required)
      {
        if (context_p->token.type == LEXER_RIGHT_BRACE)
        {
          if (context_p->stack_top_uint8 == PARSER_STATEMENT_BLOCK)
          {
            parser_stack_pop_uint8 (context_p);
            parser_stack_iterator_init (context_p, &context_p->last_statement);
            lexer_next_token (context_p);
          }
          else if (context_p->stack_top_uint8 == PARSER_STATEMENT_SWITCH
                   || context_p->stack_top_uint8 == PARSER_STATEMENT_SWITCH_NO_DEFAULT)
          {
            int has_default = (context_p->stack_top_uint8 == PARSER_STATEMENT_SWITCH);
            parser_loop_statement loop;
            parser_switch_statement switch_statement;

            parser_stack_pop_uint8 (context_p);
            parser_stack_pop (context_p, &loop, sizeof (parser_loop_statement));
            parser_stack_pop (context_p, &switch_statement, sizeof (parser_switch_statement));
            parser_stack_iterator_init (context_p, &context_p->last_statement);

            PARSER_ASSERT (switch_statement.branch_list_p == NULL);

            if (!has_default)
            {
              parser_set_branch_to_current_position (context_p, &switch_statement.default_branch);
            }

            parser_set_breaks_to_current_position (context_p, loop.branch_list_p);
            lexer_next_token (context_p);
          }
          else if (context_p->stack_top_uint8 == PARSER_STATEMENT_TRY)
          {
            parser_parse_try_statement (context_p);
          }
          else if (context_p->stack_top_uint8 == PARSER_STATEMENT_START)
          {
            if (context_p->status_flags & PARSER_IS_CLOSURE)
            {
              parser_stack_pop_uint8 (context_p);
              context_p->last_statement.current_p = NULL;
              /* There is no lexer_next_token here, since the
               * next token belongs to the parent context. */
              return;
            }
            parser_raise_error (context_p, "Misplaced '}' token.");
          }
        }
        else if (context_p->token.type == LEXER_SEMICOLON)
        {
          lexer_next_token (context_p);
        }
        else if (context_p->token.type != LEXER_EOS
                 && !context_p->token.was_newline)
        {
          parser_raise_error (context_p, "Expected ';' token.");
        }
      }

      statement_terminator_required = PARSER_FALSE;

      switch (context_p->stack_top_uint8)
      {
        case PARSER_STATEMENT_LABEL:
        {
          parser_label_statement label;

          parser_stack_pop_uint8 (context_p);
          parser_stack_pop (context_p, &label, sizeof (parser_label_statement));
          parser_stack_iterator_init (context_p, &context_p->last_statement);

          parser_set_breaks_to_current_position (context_p, label.break_list_p);
          continue;
          /* FALLTHRU */
        }

        case PARSER_STATEMENT_IF:
        {
          if (parser_parse_if_statement_end (context_p))
          {
            break;
          }
          continue;
          /* FALLTHRU */
        }

        case PARSER_STATEMENT_ELSE:
        {
          parser_if_else_statement else_statement;

          parser_stack_pop_uint8 (context_p);
          parser_stack_pop (context_p, &else_statement, sizeof (parser_if_else_statement));
          parser_stack_iterator_init (context_p, &context_p->last_statement);

          parser_set_branch_to_current_position (context_p, &else_statement.branch);
          continue;
          /* FALLTHRU */
        }

        case PARSER_STATEMENT_DO_WHILE:
        {
          parser_parse_do_while_statement_end (context_p);
          statement_terminator_required = PARSER_TRUE;
          continue;
          /* FALLTHRU */
        }

        case PARSER_STATEMENT_WHILE:
        {
          parser_parse_while_statement_end (context_p);
          continue;
          /* FALLTHRU */
        }

        case PARSER_STATEMENT_FOR:
        {
          parser_parse_for_statement_end (context_p);
          continue;
          /* FALLTHRU */
        }

        case PARSER_STATEMENT_FOR_IN:
        {
          parser_for_in_statement for_in_statement;
          parser_loop_statement loop;

          parser_stack_pop_uint8 (context_p);
          parser_stack_pop (context_p, &loop, sizeof (parser_loop_statement));
          parser_stack_pop (context_p, &for_in_statement, sizeof (parser_for_in_statement));
          parser_stack_iterator_init (context_p, &context_p->last_statement);

          parser_set_continues_to_current_position (context_p, loop.branch_list_p);

          parser_emit_cbc_ext_backward_branch (context_p,
                                               CBC_EXT_BRANCH_IF_FOR_IN_HAS_NEXT,
                                               for_in_statement.start_offset);

          parser_set_breaks_to_current_position (context_p, loop.branch_list_p);
          parser_set_branch_to_current_position (context_p, &for_in_statement.branch);
          continue;
          /* FALLTHRU */
        }

        case PARSER_STATEMENT_WITH:
        {
          parser_if_else_statement with_statement;

          parser_stack_pop_uint8 (context_p);
          parser_stack_pop (context_p, &with_statement, sizeof (parser_with_statement));
          parser_stack_iterator_init (context_p, &context_p->last_statement);

          parser_emit_cbc (context_p, CBC_CONTEXT_END);
          parser_set_branch_to_current_position (context_p, &with_statement.branch);
          continue;
          /* FALLTHRU */
        }

        default:
        {
          break;
        }
      }
      break;
    }
  }

  PARSER_ASSERT (context_p->stack_depth == 0);

  parser_stack_pop_uint8 (context_p);
  context_p->last_statement.current_p = NULL;

  if (context_p->status_flags & PARSER_IS_CLOSURE)
  {
    parser_raise_error (context_p, "Primary expression expected.");
  }
}

/**
 * Free jumps stored on the stack if a parse error is occured.
 */
void PARSER_NOINLINE
parser_free_jumps (parser_context *context_p, /**< context */
                   parser_stack_iterator iterator) /**< iterator position */
{
  while (1)
  {
    uint8_t type = parser_stack_iterator_read_uint8 (&iterator);
    parser_branch_item *branch_list_p = NULL;

    switch (type)
    {
      case PARSER_STATEMENT_START:
      {
        return;
      }

      case PARSER_STATEMENT_LABEL:
      {
        parser_label_statement label;

        parser_stack_iterator_skip (&iterator, 1);
        parser_stack_iterator_read (&iterator, &label, sizeof (parser_label_statement));
        parser_stack_iterator_skip (&iterator, sizeof (parser_label_statement));
        branch_list_p = label.break_list_p;
        break;
      }

      case PARSER_STATEMENT_SWITCH:
      case PARSER_STATEMENT_SWITCH_NO_DEFAULT:
      {
        parser_switch_statement switch_statement;
        parser_loop_statement loop;

        parser_stack_iterator_skip (&iterator, 1);
        parser_stack_iterator_read (&iterator, &loop, sizeof (parser_loop_statement));
        parser_stack_iterator_skip (&iterator, sizeof (parser_loop_statement));
        parser_stack_iterator_read (&iterator, &switch_statement, sizeof (parser_switch_statement));
        parser_stack_iterator_skip (&iterator, sizeof (parser_switch_statement));

        branch_list_p = switch_statement.branch_list_p;
        while (branch_list_p != NULL)
        {
          parser_branch_item *next_p = branch_list_p->next_p;
          parser_free (branch_list_p);
          branch_list_p = next_p;
        }
        branch_list_p = loop.branch_list_p;
        break;
      }

      case PARSER_STATEMENT_DO_WHILE:
      case PARSER_STATEMENT_WHILE:
      case PARSER_STATEMENT_FOR:
      case PARSER_STATEMENT_FOR_IN:
      {
        parser_loop_statement loop;

        parser_stack_iterator_skip (&iterator, 1);
        parser_stack_iterator_read (&iterator, &loop, sizeof (parser_loop_statement));
        parser_stack_iterator_skip (&iterator, parser_statement_length (type) - 1);
        branch_list_p = loop.branch_list_p;
        break;
      }

      default:
      {
        parser_stack_iterator_skip (&iterator, parser_statement_length (type));
        continue;
      }
    }

    while (branch_list_p != NULL)
    {
      parser_branch_item *next_p = branch_list_p->next_p;
      parser_free (branch_list_p);
      branch_list_p = next_p;
    }
  }
} /* parser_free_jumps */
