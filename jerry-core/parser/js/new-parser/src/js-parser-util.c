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

/**********************************************************************/
/* Emitting byte codes                                                */
/**********************************************************************/

/**
 * Append two bytes to the cbc stream.
 */
static void
parser_emit_two_bytes (parser_context *context_p, /**< context */
                       uint8_t first_byte, /**< first byte */
                       uint8_t second_byte) /**< second byte */
{
  size_t last_position = context_p->byte_code.last_position;

  if (last_position + 2 <= PARSER_CBC_STREAM_PAGE_SIZE)
  {
    parser_mem_page *page_p = context_p->byte_code.last_p;

    page_p->bytes[last_position] = first_byte;
    page_p->bytes[last_position + 1] = second_byte;
    context_p->byte_code.last_position = last_position + 2;
  }
  else if (last_position >= PARSER_CBC_STREAM_PAGE_SIZE)
  {
    parser_mem_page *page_p;

    parser_cbc_stream_alloc_page (context_p, &context_p->byte_code);
    page_p = context_p->byte_code.last_p;
    page_p->bytes[0] = first_byte;
    page_p->bytes[1] = second_byte;
    context_p->byte_code.last_position = 2;
  }
  else
  {
    context_p->byte_code.last_p->bytes[PARSER_CBC_STREAM_PAGE_SIZE - 1] = first_byte;
    parser_cbc_stream_alloc_page (context_p, &context_p->byte_code);
    context_p->byte_code.last_p->bytes[0] = second_byte;
    context_p->byte_code.last_position = 1;
  }
}

#define PARSER_APPEND_TO_BYTE_CODE(context_p, byte) \
  if ((context_p)->byte_code.last_position >= PARSER_CBC_STREAM_PAGE_SIZE) \
  { \
    parser_cbc_stream_alloc_page ((context_p), &(context_p)->byte_code); \
  } \
  (context_p)->byte_code.last_p->bytes[(context_p)->byte_code.last_position++] = (uint8_t) (byte)

/**
 * Append the current byte code to the stream
 */
void
parser_flush_cbc (parser_context *context_p) /**< context */
{
  uint8_t flags;

  if (context_p->last_cbc_opcode == PARSER_CBC_UNAVAILABLE)
  {
    return;
  }

  if (PARSER_IS_BASIC_OPCODE (context_p->last_cbc_opcode))
  {
    cbc_opcode opcode = context_p->last_cbc_opcode;
    flags = cbc_flags[opcode];

    PARSER_APPEND_TO_BYTE_CODE (context_p, opcode);
    context_p->byte_code_size++;
  }
  else
  {
    cbc_ext_opcode opcode = PARSER_GET_EXT_OPCODE (context_p->last_cbc_opcode);

    flags = cbc_ext_flags[opcode];
    parser_emit_two_bytes (context_p, CBC_EXT_OPCODE, opcode);
    context_p->byte_code_size += 2;
  }

  PARSER_ASSERT ((flags >> CBC_STACK_ADJUST_SHIFT) >= CBC_STACK_ADJUST_BASE
                 || (CBC_STACK_ADJUST_BASE - (flags >> CBC_STACK_ADJUST_SHIFT)) <= context_p->stack_depth);
  context_p->stack_depth += CBC_STACK_ADJUST_VALUE (flags);

  if (flags & CBC_HAS_BYTE_ARG)
  {
    uint8_t byte_argument = (uint8_t) context_p->last_cbc.u.value;

    PARSER_ASSERT (context_p->last_cbc.u.value <= CBC_MAXIMUM_BYTE_VALUE);

    if (flags & CBC_POP_STACK_BYTE_ARG)
    {
      PARSER_ASSERT (context_p->stack_depth >= byte_argument);
      context_p->stack_depth -= byte_argument;
    }

    PARSER_APPEND_TO_BYTE_CODE (context_p, byte_argument);
    context_p->byte_code_size++;
  }

  if (flags & CBC_HAS_LITERAL_ARG)
  {
    uint16_t literal_index = context_p->last_cbc.literal_index;

    parser_emit_two_bytes (context_p, literal_index & 0xff, literal_index >> 8);
    context_p->byte_code_size += 2;
  }

  if (flags & CBC_HAS_LITERAL_ARG2)
  {
    uint16_t literal_index = context_p->last_cbc.u.value;

    parser_emit_two_bytes (context_p, literal_index & 0xff, literal_index >> 8);
    context_p->byte_code_size += 2;
  }

#ifdef PARSER_DEBUG
  if (context_p->is_show_opcodes)
  {
    const char *name;

    if (PARSER_IS_BASIC_OPCODE (context_p->last_cbc_opcode))
    {
      name = cbc_names[context_p->last_cbc_opcode];
    }
    else
    {
      name = cbc_ext_names[PARSER_GET_EXT_OPCODE (context_p->last_cbc_opcode)];
    }

    printf ("  [%3d] %s", (int) context_p->stack_depth, name);

    if (flags & CBC_HAS_LITERAL_ARG)
    {
      uint16_t literal_index = context_p->last_cbc.literal_index;
      lexer_literal *literal_p = parser_list_get (&context_p->literal_pool, literal_index);
      printf (" idx:%d->", literal_index);
      util_print_literal (literal_p);
    }

    if (flags & CBC_HAS_LITERAL_ARG2)
    {
      uint16_t literal_index = context_p->last_cbc.u.value;
      lexer_literal *literal_p = parser_list_get (&context_p->literal_pool, literal_index);
      printf (" idx:%d->", literal_index);
      util_print_literal (literal_p);
    }

    if (flags & CBC_HAS_BYTE_ARG)
    {
      printf (" byte_arg:%d", (int) context_p->last_cbc.u.value);
    }

    printf ("\n");
  }
#endif /* PARSER_DEBUG */

  if (context_p->stack_depth > context_p->stack_limit)
  {
    context_p->stack_limit = context_p->stack_depth;
    if (context_p->stack_limit > PARSER_MAXIMUM_STACK_LIMIT)
    {
      parser_raise_error (context_p, "Maximum local stack limit is reached.");
    }
  }

  context_p->last_cbc_opcode = PARSER_CBC_UNAVAILABLE;
} /* parser_flush_cbc */

/**
 * Append a byte code
 */
void
parser_emit_cbc (parser_context *context_p, /**< context */
                 uint16_t opcode) /**< opcode */
{
  PARSER_ASSERT (PARSER_ARGS_EQ (opcode, 0));

  if (context_p->last_cbc_opcode != PARSER_CBC_UNAVAILABLE)
  {
    parser_flush_cbc (context_p);
  }

  context_p->last_cbc_opcode = opcode;
} /* parser_emit_byte_code */

/**
 * Append a byte code with a literal argument
 */
void
parser_emit_cbc_literal (parser_context *context_p, /**< context */
                         uint16_t opcode, /**< opcode */
                         uint16_t literal_index) /**< literal index */
{
  PARSER_ASSERT (PARSER_ARGS_EQ (opcode, CBC_HAS_LITERAL_ARG));

  if (context_p->last_cbc_opcode != PARSER_CBC_UNAVAILABLE)
  {
    parser_flush_cbc (context_p);
  }

  context_p->last_cbc_opcode = opcode;
  context_p->last_cbc.literal_index = literal_index;
  context_p->last_cbc.u.literal_type[0] = LEXER_UNKNOWN_LITERAL;
  context_p->last_cbc.u.literal_type[1] = lexer_literal_object_any;
} /* parser_emit_cbc_literal */

/**
 * Append a byte code with the current literal argument
 */
void
parser_emit_cbc_literal_from_token (parser_context *context_p, /**< context */
                                    uint16_t opcode) /**< opcode */
{
  PARSER_ASSERT (PARSER_ARGS_EQ (opcode, CBC_HAS_LITERAL_ARG));

  if (context_p->last_cbc_opcode != PARSER_CBC_UNAVAILABLE)
  {
    parser_flush_cbc (context_p);
  }

  context_p->last_cbc_opcode = opcode;
  context_p->last_cbc.literal_index = context_p->literal_object.index;
  context_p->last_cbc.u.literal_type[0] = context_p->token.literal_location.type;
  context_p->last_cbc.u.literal_type[1] = context_p->literal_object.type;
} /* parser_emit_cbc_literal_from_token */

/**
 * Append a byte code with a call argument
 */
void
parser_emit_cbc_call (parser_context *context_p, /**< context */
                      uint16_t opcode, /**< opcode */
                      size_t call_arguments) /**< number of arguments */
{
  PARSER_ASSERT (PARSER_ARGS_EQ (opcode, CBC_HAS_BYTE_ARG));
  PARSER_ASSERT (call_arguments <= CBC_MAXIMUM_BYTE_VALUE);

  if (context_p->last_cbc_opcode != PARSER_CBC_UNAVAILABLE)
  {
    parser_flush_cbc (context_p);
  }

  context_p->last_cbc_opcode = opcode;
  context_p->last_cbc.u.value = call_arguments;
} /* parser_emit_cbc_call */

/**
 * Append a byte code with a branch argument
 */
void
parser_emit_cbc_forward_branch (parser_context *context_p, /**< context */
                                uint16_t opcode, /**< opcode */
                                parser_branch *branch_p) /**< branch result */
{
  uint8_t flags;
  uint32_t extra_byte_code_increase;

  if (context_p->last_cbc_opcode != PARSER_CBC_UNAVAILABLE)
  {
    parser_flush_cbc (context_p);
  }

  if (PARSER_IS_BASIC_OPCODE (opcode))
  {
    flags = cbc_flags[opcode];
    extra_byte_code_increase = 0;
  }
  else
  {
    PARSER_APPEND_TO_BYTE_CODE (context_p, CBC_EXT_OPCODE);
    opcode = PARSER_GET_EXT_OPCODE (opcode);

    flags = cbc_ext_flags[opcode];
    extra_byte_code_increase = 1;
  }

  PARSER_ASSERT (flags & CBC_HAS_BRANCH_ARG);
  PARSER_ASSERT (CBC_BRANCH_IS_FORWARD (flags));
  PARSER_ASSERT (CBC_BRANCH_OFFSET_LENGTH (opcode) == 1);

  /* Branch opcodes never push anything onto the stack. */
  PARSER_ASSERT ((flags >> CBC_STACK_ADJUST_SHIFT) >= CBC_STACK_ADJUST_BASE
                 || (CBC_STACK_ADJUST_BASE - (flags >> CBC_STACK_ADJUST_SHIFT)) <= context_p->stack_depth);
  context_p->stack_depth += CBC_STACK_ADJUST_VALUE (flags);

#ifdef PARSER_DEBUG
  if (context_p->is_show_opcodes)
  {
    if (extra_byte_code_increase == 0)
    {
      printf ("  [%3d] %s\n", (int) context_p->stack_depth, cbc_names[opcode]);
    }
    else
    {
      printf ("  [%3d] %s\n", (int) context_p->stack_depth, cbc_ext_names[opcode]);
    }
  }
#endif /* PARSER_DEBUG */

#if PARSER_MAXIMUM_CODE_SIZE <= 65535
  opcode++;
#else /* PARSER_MAXIMUM_CODE_SIZE <= 65535 */
  opcode += 2;
#endif /* PARSER_MAXIMUM_CODE_SIZE <= 65535 */

  parser_emit_two_bytes (context_p, (uint8_t) opcode, 0);
  branch_p->page_p = context_p->byte_code.last_p;
  branch_p->offset = (context_p->byte_code.last_position - 1) | (context_p->byte_code_size << 8);

  context_p->byte_code_size += extra_byte_code_increase;

#if PARSER_MAXIMUM_CODE_SIZE <= 65535
  PARSER_APPEND_TO_BYTE_CODE (context_p, 0);
  context_p->byte_code_size += 3;
#else /* PARSER_MAXIMUM_CODE_SIZE <= 65535 */
  parser_emit_two_bytes (context_p, 0, 0);
  context_p->byte_code_size += 4;
#endif /* PARSER_MAXIMUM_CODE_SIZE <= 65535 */
} /* parser_emit_cbc_forward_branch */

/**
 * Append a branch byte code and create an item
 */
parser_branch_item *
parser_emit_cbc_forward_branch_item (parser_context *context_p, /**< context */
                                     uint16_t opcode, /**< opcode */
                                     parser_branch_item *next_p) /**< next branch */
{
  parser_branch branch;
  parser_branch_item *new_item;

  /* Since byte code insertion may throw an out-of-memory error,
   * the branch is constructed locally, and copied later. */
  parser_emit_cbc_forward_branch (context_p, opcode, &branch);

  new_item = (parser_branch_item *) parser_malloc (context_p, sizeof (parser_branch_item));
  new_item->branch = branch;
  new_item->next_p = next_p;
  return new_item;
} /* parser_emit_cbc_forward_branch_item */

/**
 * Append a byte code with a branch argument
 */
void
parser_emit_cbc_backward_branch (parser_context *context_p, /**< context */
                                 uint16_t opcode, /**< opcode */
                                 uint32_t offset) /**< destination offset */
{
  uint8_t flags;
#ifdef PARSER_DEBUG
  const char *name;
#endif /* PARSER_DEBUG */

  if (context_p->last_cbc_opcode != PARSER_CBC_UNAVAILABLE)
  {
    parser_flush_cbc (context_p);
  }

  offset = context_p->byte_code_size - offset;

  if (PARSER_IS_BASIC_OPCODE (opcode))
  {
    flags = cbc_flags[opcode];

#ifdef PARSER_DEBUG
    name = cbc_names[opcode];
#endif /* PARSER_DEBUG */
  }
  else
  {
    PARSER_APPEND_TO_BYTE_CODE (context_p, CBC_EXT_OPCODE);
    opcode = PARSER_GET_EXT_OPCODE (opcode);

    flags = cbc_ext_flags[opcode];
    context_p->byte_code_size++;

#ifdef PARSER_DEBUG
    name = cbc_ext_names[opcode];
#endif /* PARSER_DEBUG */
  }

  PARSER_ASSERT (flags & CBC_HAS_BRANCH_ARG);
  PARSER_ASSERT (CBC_BRANCH_IS_BACKWARD (flags));
  PARSER_ASSERT (CBC_BRANCH_OFFSET_LENGTH (opcode) == 1);
  PARSER_ASSERT (offset <= context_p->byte_code_size);

  /* Branch opcodes never push anything onto the stack. */
  PARSER_ASSERT ((flags >> CBC_STACK_ADJUST_SHIFT) >= CBC_STACK_ADJUST_BASE
                 || (CBC_STACK_ADJUST_BASE - (flags >> CBC_STACK_ADJUST_SHIFT)) <= context_p->stack_depth);
  context_p->stack_depth += CBC_STACK_ADJUST_VALUE (flags);

#ifdef PARSER_DEBUG
  if (context_p->is_show_opcodes)
  {
    printf ("  [%3d] %s\n", (int) context_p->stack_depth, name);
  }
#endif /* PARSER_DEBUG */

  context_p->byte_code_size += 2;
#if PARSER_MAXIMUM_CODE_SIZE <= 65535
  if (offset > 255)
  {
    opcode++;
    context_p->byte_code_size++;
  }
#else /* PARSER_MAXIMUM_CODE_SIZE <= 65535 */
  if (offset > 65535)
  {
    opcode += 2;
    context_p->byte_code_size += 2;
  }
  else if (offset > 255)
  {
    opcode++;
    context_p->byte_code_size++;
  }
#endif /* PARSER_MAXIMUM_CODE_SIZE <= 65535 */

  PARSER_APPEND_TO_BYTE_CODE (context_p, (uint8_t) opcode);

#if PARSER_MAXIMUM_CODE_SIZE > 65535
  if (offset > 65535)
  {
    PARSER_APPEND_TO_BYTE_CODE (context_p, offset >> 16);
  }
#endif /* PARSER_MAXIMUM_CODE_SIZE > 65535 */

  if (offset > 255)
  {
    PARSER_APPEND_TO_BYTE_CODE (context_p, (offset >> 8) & 0xff);
  }

  PARSER_APPEND_TO_BYTE_CODE (context_p, offset & 0xff);
} /* parser_emit_cbc_backward_branch */

#undef PARSER_CHECK_LAST_POSITION
#undef PARSER_APPEND_TO_BYTE_CODE

/**
 * Set a branch to the current byte code position
 */
void
parser_set_branch_to_current_position (parser_context *context_p, /**< context */
                                       parser_branch *branch_p) /**< branch result */
{
  uint32_t delta;
  size_t offset;
  parser_mem_page *page_p = branch_p->page_p;

  if (context_p->last_cbc_opcode != PARSER_CBC_UNAVAILABLE)
  {
    parser_flush_cbc (context_p);
  }

  PARSER_ASSERT (context_p->byte_code_size > (branch_p->offset >> 8));

  delta = context_p->byte_code_size - (branch_p->offset >> 8);
  offset = (branch_p->offset & CBC_LOWER_SEVEN_BIT_MASK);

  PARSER_ASSERT (delta <= PARSER_MAXIMUM_CODE_SIZE);

#if PARSER_MAXIMUM_CODE_SIZE <= 65535
  page_p->bytes[offset++] = (delta >> 8);
  if (offset >= PARSER_CBC_STREAM_PAGE_SIZE)
  {
    page_p = page_p->next_p;
    offset = 0;
  }
#else
  page_p->bytes[offset++] = (delta >> 16);
  if (offset >= PARSER_CBC_STREAM_PAGE_SIZE)
  {
    page_p = page_p->next_p;
    offset = 0;
  }
  page_p->bytes[offset++] = ((delta >> 8) & 0xff);
  if (offset >= PARSER_CBC_STREAM_PAGE_SIZE)
  {
    page_p = page_p->next_p;
    offset = 0;
  }
#endif
  page_p->bytes[offset++] = delta & 0xff;
} /* parser_set_branch_to_current_position */

/**
 * Set breaks to the current byte code position
 */
void
parser_set_breaks_to_current_position (parser_context *context_p, /**< context */
                                       parser_branch_item *current_p) /**< branch list */
{
  while (current_p != NULL)
  {
    parser_branch_item *next_p = current_p->next_p;

    if (!(current_p->branch.offset & CBC_HIGHEST_BIT_MASK))
    {
      parser_set_branch_to_current_position (context_p, &current_p->branch);
    }
    parser_free (current_p);
    current_p = next_p;
  }
} /* parser_set_breaks_to_current_position */

/**
 * Set continues to the current byte code position
 */
void
parser_set_continues_to_current_position (parser_context *context_p, /**< context */
                                          parser_branch_item *current_p) /**< branch list */
{
  while (current_p != NULL)
  {
    if (current_p->branch.offset & CBC_HIGHEST_BIT_MASK)
    {
      parser_set_branch_to_current_position (context_p, &current_p->branch);
    }
    current_p = current_p->next_p;
  }
} /* parser_set_continues_to_current_position */
