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

/**
 * Compute real literal indicies.
 *
 * @return length of the prefix opcodes
 */
static size_t
parser_compute_offsets (parser_context *context_p, /**< context */
                        uint16_t *ident_end, /**< end of the identifier group */
                        uint16_t literal_one_byte_limit) /**< maximum value of a literal
                                                          *   encoded in one byte */
{
  parser_list_iterator literal_iterator;
  lexer_literal *literal_p;
  size_t length = 0;
  uint16_t register_index = context_p->register_count;
  uint16_t ident_index = context_p->register_count;
  uint16_t other_index = context_p->register_count;

  parser_list_iterator_init (&context_p->literal_pool, &literal_iterator);
  while ((literal_p = (lexer_literal *) parser_list_iterator_next (&literal_iterator)))
  {
    if (literal_p->type == LEXER_IDENT_LITERAL)
    {
      if (!(literal_p->status_flags & LEXER_FLAG_FUNC_ARG))
      {
        other_index++;
        if ((literal_p->status_flags & (LEXER_FLAG_VAR | LEXER_FLAG_SLOW_VAR)) == LEXER_FLAG_VAR
            && ident_index < PARSER_MAXIMUM_NUMBER_OF_REGISTERS)
        {
          /* This var literal can be stored in a register. */
          ident_index++;
        }
      }
    }
  }

  parser_list_iterator_init (&context_p->literal_pool, &literal_iterator);
  while ((literal_p = (lexer_literal *) parser_list_iterator_next (&literal_iterator)))
  {
    if (literal_p->type != LEXER_IDENT_LITERAL)
    {
      PARSER_ASSERT (literal_p->status_flags == 0);

      if (literal_p->type == LEXER_FUNCTION_LITERAL
          && literal_p->length != PARSER_ANONYMOUS_FUNCTION)
      {
        lexer_literal *name_p = parser_list_get (&context_p->literal_pool, literal_p->length);

        PARSER_ASSERT (name_p->status_flags & LEXER_FLAG_FUNC_TARGET);

        /* The name of the function always precede the function,
         * so its real index has already been computed. */
        literal_p->length = name_p->index;

        /* Opcode byte and two literal arguments. */
        length += 3;
        if (other_index > literal_one_byte_limit)
        {
          length++;
        }
        if (name_p->index > literal_one_byte_limit)
        {
          length++;
        }
      }

      literal_p->index = other_index;
      other_index++;
      continue;
    }

    if (literal_p->status_flags & LEXER_FLAG_FUNC_ARG)
    {
      continue;
    }

    if ((literal_p->status_flags & (LEXER_FLAG_VAR | LEXER_FLAG_SLOW_VAR)) == LEXER_FLAG_VAR
        && register_index < PARSER_MAXIMUM_NUMBER_OF_REGISTERS)
    {
      /* This var literal can be stored in a register. */
      literal_p->index = register_index;
      register_index++;
    }
    else
    {
      if ((literal_p->status_flags & (LEXER_FLAG_SLOW_VAR | LEXER_FLAG_FUNC_TARGET)) == LEXER_FLAG_SLOW_VAR)
      {
        /* This var literal is not a function, and cannot be
         * stored in a register, so it needs to be initialized. */

        /* Opcode byte and a literal argument. */
        length += 2;
        if (ident_index > literal_one_byte_limit)
        {
          length++;
        }
      }

      literal_p->index = ident_index;
      ident_index++;
    }
  }

  PARSER_ASSERT (other_index == context_p->literal_count);
  PARSER_ASSERT (other_index <= PARSER_MAXIMUM_NUMBER_OF_LITERALS);

  *ident_end = ident_index;
  context_p->register_count = register_index;

  return length;
} /* parser_compute_offsets */

/**
 * Get the encoded length of a literal index.
 */
static void PARSER_INLINE
encode_two_byte_literal (uint8_t *dst_p, /**< destination buffer */
                         uint16_t literal_index, /**< literal index */
                         uint16_t literal_one_byte_limit) /**< maximum value of a literal
                                                           *   encoded in one byte */
{
  if (literal_one_byte_limit == CBC_MAXIMUM_BYTE_VALUE - 1)
  {
    PARSER_ASSERT (literal_index >= CBC_MAXIMUM_BYTE_VALUE);
    dst_p[0] = CBC_MAXIMUM_BYTE_VALUE;
    dst_p[1] = literal_index - CBC_MAXIMUM_BYTE_VALUE;
  }
  else
  {
    PARSER_ASSERT (literal_index > CBC_LOWER_SEVEN_BIT_MASK);
    dst_p[0] = (uint8_t) (literal_index >> 8) | CBC_HIGHEST_BIT_MASK;
    dst_p[1] = (uint8_t) (literal_index & 0xff);
  }
} /* parser_compute_encoded_length */

/**
 * Generate initializer byte codes.
 *
 * @return the end of the initializer stream
 */
static uint8_t *
parser_generate_initializers (parser_context *context_p, /**< context */
                              uint8_t *dst_p, /**< destination buffer */
                              literal_value *literal_pool_p, /**< start of literal pool */
                              uint16_t literal_one_byte_limit) /**< maximum value of a literal
                                                                *   encoded in one byte */
{
  parser_list_iterator literal_iterator;
  lexer_literal *literal_p;

  parser_list_iterator_init (&context_p->literal_pool, &literal_iterator);
  while ((literal_p = (lexer_literal *) parser_list_iterator_next (&literal_iterator)))
  {
    literal_pool_p[literal_p->index] = literal_p->value;

    if (literal_p->type == LEXER_FUNCTION_LITERAL
        && literal_p->length != PARSER_ANONYMOUS_FUNCTION)
    {
      *dst_p++ = CBC_DEFINE_FUNCTION;

      if (literal_p->length <= literal_one_byte_limit)
      {
        *dst_p++ = (uint8_t) literal_p->length;
      }
      else
      {
        encode_two_byte_literal (dst_p, literal_p->length, literal_one_byte_limit);
        dst_p += 2;
      }

      if (literal_p->index <= literal_one_byte_limit)
      {
        *dst_p++ = (uint8_t) literal_p->index;
      }
      else
      {
        encode_two_byte_literal (dst_p, literal_p->index, literal_one_byte_limit);
        dst_p += 2;
      }
    }

    if (literal_p->type == LEXER_IDENT_LITERAL
        && (literal_p->status_flags & LEXER_FLAG_SLOW_VAR)
        && !(literal_p->status_flags & LEXER_FLAG_FUNC_TARGET))
    {
      PARSER_ASSERT (!(literal_p->status_flags & LEXER_FLAG_FUNC_ARG));

      *dst_p++ = CBC_DEFINE_VAR;
      if (literal_p->index <= literal_one_byte_limit)
      {
        *dst_p++ = (uint8_t) literal_p->index;
      }
      else
      {
        encode_two_byte_literal (dst_p, literal_p->index, literal_one_byte_limit);
        dst_p += 2;
      }
    }
  }
  return dst_p;
} /* parser_generate_initializers */

/*
 * During byte code post processing certain bytes are not
 * copied into the final byte code buffer. For example, if
 * one byte is enough for encoding a literal index, the
 * second byte is not copied. However, when a byte is skipped,
 * the offsets of those branches which crosses (jumps over)
 * that byte code should also be decreased by one. Instead
 * of finding these jumps every time when a byte is skipped,
 * all branch offset updates are computed in one step.
 *
 * Branch offset mapping example:
 *
 * Let's assume that each parser_mem_page of the byte_code
 * buffer is 8 bytes long and only 4 bytes are kept for a
 * given page:
 *
 * +---+---+---+---+---+---+---+---+
 * | X | 1 | 2 | 3 | X | 4 | X | X |
 * +---+---+---+---+---+---+---+---+
 *
 * X marks those bytes which are removed. The resulting
 * offset mapping is the following:
 *
 * +---+---+---+---+---+---+---+---+
 * | 0 | 1 | 2 | 3 | 3 | 4 | 4 | 4 |
 * +---+---+---+---+---+---+---+---+
 *
 * Each X is simply replaced by the index of the previous
 * index starting from zero. This shows the number of
 * copied bytes before a given byte including the byte
 * itself. The last byte always shows the number of bytes
 * copied from this page.
 *
 * This mapping allows recomputing all branch targets,
 * since mapping[to] - mapping[from] is the new argument
 * for forward branches. As for backward branches, the
 * equation is reversed to mapping[from] - mapping[to].
 *
 * The mapping is relative to one page, so distance
 * computation affecting multiple pages requires a loop.
 * We should also note that only argument bytes can
 * be skipped, so removed bytes cannot be targeted by
 * branches. Valid branches always target instruction
 * starts only.
 */

/**
 * Recompute the argument of a forward branch.
 *
 * @return the new distance
 */
static size_t
parser_update_forward_branch (parser_mem_page *page_p, /**< current page */
                              size_t full_distance, /**< full distance */
                              uint8_t bytes_copied_before_jump) /**< bytes copied before jump */
{
  size_t new_distance = 0;

  while (full_distance > PARSER_CBC_STREAM_PAGE_SIZE)
  {
    new_distance += page_p->bytes[PARSER_CBC_STREAM_PAGE_SIZE - 1] & CBC_LOWER_SEVEN_BIT_MASK;
    full_distance -= PARSER_CBC_STREAM_PAGE_SIZE;
    page_p = page_p->next_p;
  }

  new_distance += page_p->bytes[full_distance - 1] & CBC_LOWER_SEVEN_BIT_MASK;
  return new_distance - bytes_copied_before_jump;
} /* parser_update_forward_branch */

/**
 * Recompute the argument of a backward branch.
 *
 * @return the new distance
 */
static size_t
parser_update_backward_branch (parser_mem_page *page_p, /**< current page */
                               size_t full_distance, /**< full distance */
                               uint8_t bytes_copied_before_jump) /**< bytes copied before jump */
{
  size_t new_distance = bytes_copied_before_jump;

  while (full_distance >= PARSER_CBC_STREAM_PAGE_SIZE)
  {
    PARSER_ASSERT (page_p != NULL);
    new_distance += page_p->bytes[PARSER_CBC_STREAM_PAGE_SIZE - 1] & CBC_LOWER_SEVEN_BIT_MASK;
    full_distance -= PARSER_CBC_STREAM_PAGE_SIZE;
    page_p = page_p->next_p;
  }

  if (full_distance > 0)
  {
    size_t offset = PARSER_CBC_STREAM_PAGE_SIZE - full_distance;

    PARSER_ASSERT (page_p != NULL);

    new_distance += page_p->bytes[PARSER_CBC_STREAM_PAGE_SIZE - 1] & CBC_LOWER_SEVEN_BIT_MASK;
    new_distance -= page_p->bytes[offset - 1] & CBC_LOWER_SEVEN_BIT_MASK;
  }

  return new_distance;
} /* parser_update_backward_branch */

/**
 * Update targets of all branches in one step.
 */
static void
parse_update_branches (parser_context *context_p, /**< context */
                       uint8_t *byte_code_p, /**< byte code */
                       size_t length) /**< length of byte code */
{
  parser_mem_page *page_p = context_p->byte_code.first_p;
  parser_mem_page *prev_page_p = NULL;
  parser_mem_page *last_page_p = context_p->byte_code.last_p;
  size_t last_position = context_p->byte_code.last_position;
  size_t offset = 0;
  size_t bytes_copied = 0;

  if (last_position >= PARSER_CBC_STREAM_PAGE_SIZE)
  {
    last_page_p = NULL;
    last_position = 0;
  }

  while (page_p != last_page_p || offset < last_position)
  {
    /* Branch instructions are marked to improve search speed. */
    if (page_p->bytes[offset] & CBC_HIGHEST_BIT_MASK)
    {
      uint8_t *bytes_p = byte_code_p + bytes_copied;
      uint8_t flags;
      uint8_t bytes_copied_before_jump = 0;
      size_t branch_argument_length;
      size_t target_distance;
      size_t length;

      if (offset > 0)
      {
        bytes_copied_before_jump = page_p->bytes[offset - 1] & CBC_LOWER_SEVEN_BIT_MASK;
      }
      bytes_p += bytes_copied_before_jump;

      if (*bytes_p == CBC_EXT_OPCODE)
      {
        bytes_p++;
        flags = cbc_ext_flags[*bytes_p];
      }
      else
      {
        flags = cbc_flags[*bytes_p];
      }

      PARSER_ASSERT (flags & CBC_HAS_BRANCH_ARG);
      branch_argument_length = CBC_BRANCH_OFFSET_LENGTH (*bytes_p);
      bytes_p++;

      /* Decoding target. */
      length = branch_argument_length;
      target_distance = 0;
      do
      {
        target_distance = (target_distance << 8) | *bytes_p;
        bytes_p++;
      }
      while (--length > 0);

      if (CBC_BRANCH_IS_FORWARD (flags))
      {
        /* Branch target was not set. */
        PARSER_ASSERT (target_distance > 0);

        target_distance = parser_update_forward_branch (page_p,
                                                        offset + target_distance,
                                                        bytes_copied_before_jump);
      }
      else
      {
        if (target_distance < offset)
        {
          uint8_t bytes_copied_before_target = page_p->bytes[offset - target_distance - 1];
          target_distance = bytes_copied_before_jump - (bytes_copied_before_target & CBC_LOWER_SEVEN_BIT_MASK);
        }
        else if (target_distance == offset)
        {
          target_distance = bytes_copied_before_jump;
        }
        else
        {
          target_distance = parser_update_backward_branch (prev_page_p,
                                                           target_distance - offset,
                                                           bytes_copied_before_jump);
        }
      }

      /* Encoding target again. */
      do
      {
        bytes_p--;
        *bytes_p = (uint8_t) (target_distance & 0xff);
        target_distance >>= 8;
      }
      while (--branch_argument_length > 0);
    }

    offset++;
    if (offset >= PARSER_CBC_STREAM_PAGE_SIZE)
    {
      parser_mem_page *next_p = page_p->next_p;

      /* We reverse the pages before the current page. */
      page_p->next_p = prev_page_p;
      prev_page_p = page_p;

      bytes_copied += page_p->bytes[PARSER_CBC_STREAM_PAGE_SIZE - 1] & CBC_LOWER_SEVEN_BIT_MASK;
      page_p = next_p;
      offset = 0;
    }
  }

  /* After this point the pages of the byte code stream are
   * not used anymore. However, they needs to be freed during
   * cleanup, so the first and last pointers of the stream
   * descriptor are reversed as well. */
  if (last_page_p != NULL)
  {
    PARSER_ASSERT (last_page_p == context_p->byte_code.last_p);
    last_page_p->next_p = prev_page_p;
  }
  else
  {
    last_page_p = context_p->byte_code.last_p;
  }

  context_p->byte_code.last_p = context_p->byte_code.first_p;
  context_p->byte_code.first_p = last_page_p;
} /* parse_update_branches */

#ifdef PARSER_DEBUG

static void
parse_print_final_cbc (cbc_compiled_code *compiled_code_p, /* compiled code */
                       parser_list *literal_pool_p, /* literal pool */
                       size_t length) /**< length of byte code */
{
  cbc_opcode opcode;
  cbc_opcode ext_opcode;
  uint8_t flags;
  uint8_t *byte_code_start_p;
  uint8_t *byte_code_end_p;
  uint8_t *byte_code_p;
  uint16_t encoding_limit;
  uint16_t encoding_delta;

  printf ("\nFinal byte code dump:\n\n  Maximum stack depth: %d\n", (int) compiled_code_p->stack_limit);
  printf ("  Literal encoding: ");
  if (compiled_code_p->literal_encoding == cbc_literal_encoding_small)
  {
    printf ("small\n");
    encoding_limit = 255;
    encoding_delta = 0xfe01;
  }
  else
  {
    printf ("full\n");
    encoding_limit = 128;
    encoding_delta = 0x8000;
  }

  printf ("  Argument range end: %d\n",
          (int) compiled_code_p->argument_end);
  printf ("  Register range end: %d\n",
          (int) compiled_code_p->register_end);
  printf ("  Identifier range end: %d\n",
          (int) compiled_code_p->ident_end);
  printf ("  Literal range end: %d\n\n",
          (int) compiled_code_p->literal_end);

  byte_code_start_p = ((uint8_t *) compiled_code_p) + sizeof (cbc_compiled_code);
  byte_code_start_p += compiled_code_p->literal_end * sizeof (literal_value);
  byte_code_end_p = byte_code_start_p + length;
  byte_code_p = byte_code_start_p;

  while (byte_code_p < byte_code_end_p)
  {
    size_t cbc_offset;

    opcode = *byte_code_p;
    ext_opcode = CBC_EXT_NOP;
    cbc_offset = byte_code_p - byte_code_start_p;

    if (opcode != CBC_EXT_OPCODE)
    {
      flags = cbc_flags[opcode];
      printf (" %3d : %s", (int) cbc_offset, cbc_names[opcode]);
      byte_code_p++;
    }
    else
    {
      ext_opcode = byte_code_p[1];
      flags = cbc_ext_flags[ext_opcode];
      printf (" %3d : %s", (int) cbc_offset, cbc_ext_names[ext_opcode]);
      byte_code_p += 2;
    }

    if (flags & CBC_HAS_BYTE_ARG)
    {
      printf (" byte_arg:%d", *byte_code_p);
      byte_code_p++;
    }

    while (flags & (CBC_HAS_LITERAL_ARG | CBC_HAS_LITERAL_ARG2))
    {
      uint16_t literal_index = *byte_code_p;
      parser_list_iterator literal_iterator;
      lexer_literal *literal_p;

      /* Fast literal encoding. */
      byte_code_p++;
      if (literal_index >= encoding_limit)
      {
        literal_index = ((literal_index << 8) | byte_code_p[0]) - encoding_delta;
        byte_code_p++;
      }

      parser_list_iterator_init (literal_pool_p, &literal_iterator);
      while (1)
      {
        literal_p = (lexer_literal *) parser_list_iterator_next (&literal_iterator);
        PARSER_ASSERT (literal_p != NULL);

        if (literal_p->index == literal_index)
        {
          printf (" idx:%d", literal_index);

          if (literal_index < compiled_code_p->argument_end)
          {
            printf ("(arg)->");
          }
          else if (literal_index < compiled_code_p->register_end)
          {
            printf ("(reg)->");
          }
          else if (literal_index < compiled_code_p->ident_end)
          {
            printf ("(ident)->");
          }
          else
          {
            printf ("(lit)->");
          }

          util_print_literal (literal_p);
          break;
        }
      }

      if (flags & CBC_HAS_LITERAL_ARG)
      {
        flags &= ~CBC_HAS_LITERAL_ARG;
      }
      else
      {
        break;
      }
    }

    if (flags & CBC_HAS_BRANCH_ARG)
    {
      size_t branch_offset_length = CBC_BRANCH_OFFSET_LENGTH (opcode);
      size_t offset = 0;

      if (opcode == CBC_EXT_OPCODE)
      {
        branch_offset_length = CBC_BRANCH_OFFSET_LENGTH (ext_opcode);
      }

      do
      {
        offset = (offset << 8) | *byte_code_p++;
      }
      while (--branch_offset_length > 0);

      if (CBC_BRANCH_IS_FORWARD (flags))
      {
        printf (" offset:%d(->%d)", (int) offset, (int) (cbc_offset + offset));
      }
      else
      {
        printf (" offset:%d(->%d)", (int) offset, (int) (cbc_offset - offset));
      }
    }
    printf ("\n");
  }
} /* parse_print_final_cbc */

#endif /* PARSER_DEBUG */

#define PARSER_NEXT_BYTE(page_p, offset) \
  do { \
    if (++(offset) >= PARSER_CBC_STREAM_PAGE_SIZE) \
    { \
      offset = 0; \
      page_p = page_p->next_p; \
    } \
  } while (0)

#define PARSER_NEXT_BYTE_UPDATE(page_p, offset, real_offset) \
  do { \
    page_p->bytes[offset] = real_offset; \
    if (++(offset) >= PARSER_CBC_STREAM_PAGE_SIZE) \
    { \
      offset = 0; \
      real_offset = 0; \
      page_p = page_p->next_p; \
    } \
  } while (0)

/**
 * Post processing main function.
 *
 * @return compiled code
 */
static cbc_compiled_code *
parser_post_processing (parser_context *context_p) /**< context */
{
  cbc_literal_encoding literal_encoding;
  uint16_t literal_one_byte_limit;
  uint16_t ident_end;
  parser_mem_page *page_p;
  parser_mem_page *last_page_p = context_p->byte_code.last_p;
  size_t last_position = context_p->byte_code.last_position;
  size_t offset;
  size_t length;
  size_t total_size;
  size_t initializers_length;
  uint8_t real_offset;
  uint8_t *byte_code_p;
  cbc_compiled_code *compiled_code_p;
  literal_value *literal_pool_p;
  uint8_t *dst_p;

  if ((size_t) context_p->stack_limit + (size_t) context_p->register_count > PARSER_MAXIMUM_STACK_LIMIT)
  {
    parser_raise_error (context_p, "Maximum local stack limit is reached.");
  }

  PARSER_ASSERT (context_p->literal_count <= PARSER_MAXIMUM_NUMBER_OF_LITERALS);

  if (context_p->literal_count <= CBC_MAXIMUM_SMALL_VALUE)
  {
    literal_encoding = cbc_literal_encoding_small;
    literal_one_byte_limit = CBC_MAXIMUM_BYTE_VALUE - 1;
  }
  else
  {
    literal_encoding = cbc_literal_encoding_full;
    literal_one_byte_limit = CBC_LOWER_SEVEN_BIT_MASK;
  }

  initializers_length = parser_compute_offsets (context_p, &ident_end, literal_one_byte_limit);
  length = initializers_length;

  if (last_position >= PARSER_CBC_STREAM_PAGE_SIZE)
  {
    last_page_p = NULL;
    last_position = 0;
  }

  page_p = context_p->byte_code.first_p;
  offset = 0;

  while (page_p != last_page_p || offset < last_position)
  {
    uint8_t *opcode_p;
    cbc_opcode opcode;
    uint8_t flags;
    size_t branch_offset_length;

    opcode_p = page_p->bytes + offset;
    opcode = *opcode_p;
    PARSER_NEXT_BYTE (page_p, offset);
    branch_offset_length = CBC_BRANCH_OFFSET_LENGTH (opcode);
    flags = cbc_flags[opcode];
    length++;

    if (opcode == CBC_EXT_OPCODE)
    {
      cbc_ext_opcode ext_opcode;

      ext_opcode = page_p->bytes[offset];
      branch_offset_length = CBC_BRANCH_OFFSET_LENGTH (ext_opcode);
      flags = cbc_ext_flags[ext_opcode];
      PARSER_NEXT_BYTE (page_p, offset);
      length++;
    }

    if (flags & CBC_HAS_BYTE_ARG)
    {
      /* This argument will be copied without modification. */
      PARSER_NEXT_BYTE (page_p, offset);
      length++;
    }

    while (flags & (CBC_HAS_LITERAL_ARG | CBC_HAS_LITERAL_ARG2))
    {
      uint8_t *first_byte = page_p->bytes + offset;
      size_t literal_index = *first_byte;
      lexer_literal *literal_p;

      PARSER_NEXT_BYTE (page_p, offset);
      length++;

      literal_index |= ((size_t) page_p->bytes[offset]) << 8;
      literal_p = parser_list_get (&context_p->literal_pool, literal_index);

      if (literal_p->index <= literal_one_byte_limit)
      {
        *first_byte = (uint8_t) literal_p->index;
      }
      else
      {
        if (literal_encoding == cbc_literal_encoding_small)
        {
          PARSER_ASSERT (literal_index <= CBC_MAXIMUM_SMALL_VALUE);
          *first_byte = CBC_MAXIMUM_BYTE_VALUE;
          page_p->bytes[offset] = (uint8_t) (literal_p->index - CBC_MAXIMUM_BYTE_VALUE);
          length++;
        }
        else
        {
          PARSER_ASSERT (literal_index <= CBC_MAXIMUM_FULL_VALUE);
          *first_byte = (uint8_t) (literal_p->index >> 8) | CBC_HIGHEST_BIT_MASK;
          page_p->bytes[offset] = (uint8_t) (literal_p->index & 0xff);
          length++;
        }
      }
      PARSER_NEXT_BYTE (page_p, offset);

      if (flags & CBC_HAS_LITERAL_ARG)
      {
        flags &= ~CBC_HAS_LITERAL_ARG;
      }
      else
      {
        break;
      }
    }

    if (flags & CBC_HAS_BRANCH_ARG)
    {
      int prefix_zero = PARSER_TRUE;
#if PARSER_MAXIMUM_CODE_SIZE <= 65535
      cbc_opcode jump_forward = CBC_JUMP_FORWARD_2;
#else /* PARSER_MAXIMUM_CODE_SIZE <= 65535 */
      cbc_opcode jump_forward = CBC_JUMP_FORWARD_3;
#endif /* PARSER_MAXIMUM_CODE_SIZE <= 65535 */

      /* The leading zeroes are dropped from the stream.
       * Although dropping these zeroes for backward
       * branches are unnecessary, we use the same
       * code path for simplicity. */
      PARSER_ASSERT (branch_offset_length > 0 && branch_offset_length <= 3);

      while (--branch_offset_length > 0)
      {
        uint8_t byte = page_p->bytes[offset];
        if (byte > 0 || !prefix_zero)
        {
          prefix_zero = PARSER_FALSE;
          length++;
        }
        else
        {
          PARSER_ASSERT (CBC_BRANCH_IS_FORWARD (flags));
        }
        PARSER_NEXT_BYTE (page_p, offset);
      }

      if (opcode == jump_forward
          && prefix_zero
          && page_p->bytes[offset] == CBC_BRANCH_OFFSET_LENGTH (jump_forward) + 1)
      {
        /* Uncoditional jumps which jump right after the instruction
         * are effectively NOPs. These jumps are removed from the
         * stream. The 1 byte long CBC_JUMP_FORWARD form marks these
         * instructions, since this form is constructed during post
         * processing and cannot be emitted directly. */
        *opcode_p = CBC_JUMP_FORWARD;
        length --;
      }
      else
      {
        /* Other last bytes are always copied. */
        length++;
      }

      PARSER_NEXT_BYTE (page_p, offset);
    }
  }

  length++; /* TODO auto-detection of return opcode */

  total_size = sizeof (cbc_compiled_code) + length;
  total_size += context_p->literal_count * sizeof (literal_value);
  compiled_code_p = (cbc_compiled_code *) parser_malloc (context_p, total_size);

  compiled_code_p->stack_limit = context_p->register_count + context_p->stack_limit;
  compiled_code_p->argument_end = context_p->argument_count;
  compiled_code_p->register_end = context_p->register_count;
  compiled_code_p->ident_end = ident_end;
  compiled_code_p->literal_end = context_p->literal_count;
  compiled_code_p->literal_encoding = literal_encoding;

  byte_code_p = ((uint8_t *) compiled_code_p) + sizeof (cbc_compiled_code);
  literal_pool_p = (literal_value *) byte_code_p;
  byte_code_p += context_p->literal_count * sizeof (literal_value);

  dst_p = parser_generate_initializers (context_p,
                                        byte_code_p,
                                        literal_pool_p,
                                        literal_one_byte_limit);

  PARSER_ASSERT (dst_p == byte_code_p + initializers_length);

  page_p = context_p->byte_code.first_p;
  offset = 0;
  real_offset = 0;

  while (page_p != last_page_p || offset < last_position)
  {
    uint8_t flags;
    uint8_t *opcode_p;
    uint8_t *branch_mark_p;
    cbc_opcode opcode;
    size_t branch_offset_length;

    opcode_p = dst_p;
    branch_mark_p = page_p->bytes + offset;
    opcode = *branch_mark_p;
    branch_offset_length = CBC_BRANCH_OFFSET_LENGTH (opcode);

    if (opcode == CBC_JUMP_FORWARD)
    {
      /* These opcodes are deleted from the stream. */
#if PARSER_MAXIMUM_CODE_SIZE <= 65535
      size_t length = 3;
#else /* PARSER_MAXIMUM_CODE_SIZE <= 65535 */
      size_t length = 4;
#endif /* PARSER_MAXIMUM_CODE_SIZE <= 65535 */

      do
      {
        PARSER_NEXT_BYTE_UPDATE (page_p, offset, real_offset);
      }
      while (--length > 0);

      continue;
    }

    /* Storing the opcode */
    *dst_p++ = opcode;
    real_offset++;
    PARSER_NEXT_BYTE_UPDATE (page_p, offset, real_offset);
    flags = cbc_flags[opcode];

    if (opcode == CBC_EXT_OPCODE)
    {
      cbc_ext_opcode ext_opcode;

      ext_opcode = page_p->bytes[offset];
      flags = cbc_ext_flags[ext_opcode];
      branch_offset_length = CBC_BRANCH_OFFSET_LENGTH (ext_opcode);

      /* Storing the extended opcode */
      *dst_p++ = ext_opcode;
      opcode_p++;
      real_offset++;
      PARSER_NEXT_BYTE_UPDATE (page_p, offset, real_offset);
    }

    if (flags & CBC_HAS_BRANCH_ARG)
    {
      *branch_mark_p |= CBC_HIGHEST_BIT_MASK;
    }

    /* Only literal and call arguments can be combined. */
    PARSER_ASSERT (!(flags & CBC_HAS_BRANCH_ARG)
                   || !(flags & (CBC_HAS_BYTE_ARG | CBC_HAS_LITERAL_ARG)));

    if (flags & CBC_HAS_BYTE_ARG)
    {
      /* This argument will be copied without modification. */
      *dst_p++ = page_p->bytes[offset];
      real_offset++;
      PARSER_NEXT_BYTE_UPDATE (page_p, offset, real_offset);
    }

    while (flags & (CBC_HAS_LITERAL_ARG | CBC_HAS_LITERAL_ARG2))
    {
      uint8_t first_byte = page_p->bytes[offset];

      *dst_p++ = first_byte;
      real_offset++;
      PARSER_NEXT_BYTE_UPDATE (page_p, offset, real_offset);

      if (first_byte > literal_one_byte_limit)
      {
        *dst_p++ = page_p->bytes[offset];
        real_offset++;
      }
      PARSER_NEXT_BYTE_UPDATE (page_p, offset, real_offset);

      if (flags & CBC_HAS_LITERAL_ARG)
      {
        flags &= ~CBC_HAS_LITERAL_ARG;
      }
      else
      {
        break;
      }
    }

    if (flags & CBC_HAS_BRANCH_ARG)
    {
      int prefix_zero = PARSER_TRUE;

      /* The leading zeroes are dropped from the stream. */
      PARSER_ASSERT (branch_offset_length > 0 && branch_offset_length <= 3);

      while (--branch_offset_length > 0)
      {
        uint8_t byte = page_p->bytes[offset];
        if (byte > 0 || !prefix_zero)
        {
          prefix_zero = PARSER_FALSE;
          *dst_p++ = page_p->bytes[offset];
          real_offset++;
        }
        else
        {
          /* When a leading zero is dropped, the branch
           * offset length must be decreased as well. */
          (*opcode_p)--;
        }
        PARSER_NEXT_BYTE_UPDATE (page_p, offset, real_offset);
      }

      *dst_p++ = page_p->bytes[offset];
      real_offset++;
      PARSER_NEXT_BYTE_UPDATE (page_p, offset, real_offset);
    }
  }

  *dst_p++ = CBC_RETURN_WITH_UNDEFINED;
  PARSER_ASSERT (dst_p == byte_code_p + length);

  parse_update_branches (context_p,
                         byte_code_p + initializers_length,
                         length - initializers_length);

#ifdef PARSER_DEBUG
  if (context_p->is_show_opcodes)
  {
    parse_print_final_cbc (compiled_code_p, &context_p->literal_pool, length);
    printf ("\nByte code size: %d bytes\n", (int) length);
    context_p->total_byte_code_size += length;
  }
#endif

  return compiled_code_p;
} /* parser_post_processing */

#undef PARSER_NEXT_BYTE
#undef PARSER_NEXT_BYTE_UPDATE

/**
 * Free identifiers and literals.
 */
static void
parser_free_literals (parser_context *context_p, /**< context */
                      parser_list *literal_pool_p) /**< literals */
{
  parser_list_iterator literal_iterator;
  lexer_literal *literal_p;

  parser_list_iterator_init (literal_pool_p, &literal_iterator);
  while ((literal_p = (lexer_literal *) parser_list_iterator_next (&literal_iterator)) != NULL)
  {
    util_free_literal (literal_p);
  }

  parser_list_free (literal_pool_p);
} /* parser_free_literals */

/**
 * Parse EcmaScript source code
 */
void
parser_parse_script (const uint8_t *source_p, size_t size)
{
  parser_context context;

  context.error_str_p = NULL;
  context.allocated_buffer_p = NULL;

  context.status_flags = 0;
  context.stack_depth = 0;
  context.stack_limit = 0;
  context.last_context_p = NULL;
  context.last_statement.current_p = NULL;
#ifdef PARSER_DEBUG
  context.is_show_opcodes = 1;
  context.total_byte_code_size = 0;
#endif

  context.source_p = source_p;
  context.source_end_p = source_p + size;
  context.line = 1;
  context.column = 1;

  context.last_cbc_opcode = PARSER_CBC_UNAVAILABLE;

  context.argument_count = 0;
  context.register_count = 0;
  context.literal_count = 0;

  parser_cbc_stream_init (&context.byte_code);
  context.byte_code_size = 0;
  parser_list_init (&context.literal_pool, sizeof (lexer_literal), 15);
  parser_stack_init (&context);

#ifdef PARSER_DEBUG
  if (context.is_show_opcodes)
  {
    printf ("\n--- Script parsing start ---\n\n");
  }
#endif /* PARSER_DEBUG */

  PARSER_TRY (context.try_buffer)
  {
    /* Pushing a dummy value ensures the stack is never empty.
     * This simplifies the stack management routines. */
    parser_stack_push_uint8 (&context, CBC_MAXIMUM_BYTE_VALUE);
    /* The next token must always be present to make decisions
     * in the parser. Therefore when a token is consumed, the
     * lexer_next_token() must be immediately called. */
    lexer_next_token (&context);

    parser_parse_statements (&context);

    /* When the parsing is successful, only the
     * dummy value can be remained on the stack. */
    PARSER_ASSERT (context.stack_top_uint8 == CBC_MAXIMUM_BYTE_VALUE
                   && context.stack.last_position == 1
                   && context.stack.first_p != NULL
                   && context.stack.first_p->next_p == NULL
                   && context.stack.last_p == NULL);
    PARSER_ASSERT (context.last_statement.current_p == NULL);

    PARSER_ASSERT (context.last_cbc_opcode == PARSER_CBC_UNAVAILABLE);
    PARSER_ASSERT (context.allocated_buffer_p == NULL);

    parser_post_processing (&context);

#ifdef PARSER_DEBUG
    if (context.is_show_opcodes)
    {
      printf ("\nScript parsing successfully completed. Total byte code size: %d bytes\n",
              (int) context.total_byte_code_size);
    }
#endif /* PARSER_DEBUG */
  }
  PARSER_CATCH
  {
    if (context.last_statement.current_p != NULL)
    {
      parser_free_jumps (&context, context.last_statement);
    }

    if (context.allocated_buffer_p != NULL)
    {
      parser_free_local (context.allocated_buffer_p);
    }

#ifdef PARSER_DEBUG
    if (context.is_show_opcodes)
    {
      printf ("Parse error '%s' at line: %d col: %d\n",
              context.error_str_p,
              (int) context.token.line,
              (int) context.token.column);
    }
#endif /* PARSER_DEBUG */
  }
  PARSER_TRY_END

#ifdef PARSER_DEBUG
  if (context.is_show_opcodes)
  {
    printf ("\n--- Script parsing end ---\n\n");
  }
#endif /* PARSER_DEBUG */

  parser_cbc_stream_free (&context.byte_code);
  parser_free_literals (&context, &context.literal_pool);
  parser_stack_free (&context);
} /* parser_parse_script */

/**
 * Parse function code
 *
 * @return compiled code
 */
cbc_compiled_code *
parser_parse_function (parser_context *context_p, /**< context */
                       uint32_t status_flags) /**< extra status flags */
{
  parser_saved_context saved_context;
  cbc_compiled_code *compiled_code_p;

  PARSER_ASSERT (context_p->last_cbc_opcode == PARSER_CBC_UNAVAILABLE);

  /* Save private part of the context. */

  saved_context.status_flags = context_p->status_flags;
  saved_context.stack_depth = context_p->stack_depth;
  saved_context.stack_limit = context_p->stack_limit;
  saved_context.prev_context_p = context_p->last_context_p;
  saved_context.last_statement = context_p->last_statement;

  saved_context.argument_count = context_p->argument_count;
  saved_context.register_count = context_p->register_count;
  saved_context.literal_count = context_p->literal_count;

  saved_context.byte_code = context_p->byte_code;
  saved_context.byte_code_size = context_p->byte_code_size;
  saved_context.literal_pool_data = context_p->literal_pool.data;

  /* Reset private part of the context. */

  PARSER_ASSERT (status_flags & PARSER_IS_FUNCTION);

  context_p->status_flags &= PARSER_IS_STRICT;
  context_p->status_flags |= status_flags;
  context_p->stack_depth = 0;
  context_p->stack_limit = 0;
  context_p->last_context_p = &saved_context;
  context_p->last_statement.current_p = NULL;

  context_p->argument_count = 0;
  context_p->register_count = 0;
  context_p->literal_count = 0;

  parser_cbc_stream_init (&context_p->byte_code);
  context_p->byte_code_size = 0;
  parser_list_reset (&context_p->literal_pool);

#ifdef PARSER_DEBUG
  if (context_p->is_show_opcodes)
  {
    printf ("\n--- Function parsing start ---\n\n");
  }
#endif /* PARSER_DEBUG */

  lexer_next_token (context_p);

  if (context_p->token.type != LEXER_LEFT_PAREN)
  {
    parser_raise_error (context_p, "Expected argument list.");
  }

  lexer_next_token (context_p);

  /* Argument parsing. */
  if (context_p->token.type != LEXER_RIGHT_PAREN)
  {
    while (PARSER_TRUE)
    {
      uint16_t literal_count = context_p->literal_count;

      if (context_p->token.type != LEXER_LITERAL
          || context_p->token.literal_location.type != LEXER_IDENT_LITERAL)
      {
        parser_raise_error (context_p, "Argument name expected.");
      }

      lexer_construct_literal_object (context_p,
                                      &context_p->token.literal_location,
                                      LEXER_IDENT_LITERAL);

      if (context_p->token.literal_is_reserved
          || context_p->literal_object.type != lexer_literal_object_any)
      {
        context_p->status_flags |= PARSER_HAS_NON_STRICT_ARG;
      }

      context_p->literal_object.literal_p->index = literal_count;
      context_p->literal_object.literal_p->status_flags = LEXER_FLAG_FUNC_ARG;

      if (literal_count == context_p->literal_count)
      {
        lexer_literal *literal_p;

        /* We create an empty record for repeated arguments. */
        literal_p = (lexer_literal *) parser_list_append (context_p, &context_p->literal_pool);
        literal_p->length = 0;
        literal_p->index = literal_count;
        literal_p->type = LEXER_IDENT_LITERAL;
        literal_p->status_flags = LEXER_FLAG_FUNC_ARG;

        if (util_set_char_literal (literal_p, NULL) != 0)
        {
          parser_raise_error (context_p, "Out of memory.");
        }
        context_p->status_flags |= PARSER_HAS_NON_STRICT_ARG;
        context_p->literal_count++;
      }

      /* Same as literal_count + 1 > PARSER_MAXIMUM_NUMBER_OF_REGISTERS. */
      if (literal_count >= PARSER_MAXIMUM_NUMBER_OF_REGISTERS)
      {
        parser_raise_error (context_p, "Maximum number of registers is reached.");
      }

      lexer_next_token (context_p);

      if (context_p->token.type != LEXER_COMMA)
      {
        break;
      }

      lexer_next_token (context_p);
    }
  }

  if (context_p->token.type != LEXER_RIGHT_PAREN)
  {
    parser_raise_error (context_p, "Expected ')' token.");
  }

  lexer_next_token (context_p);

  context_p->register_count = context_p->literal_count;
  context_p->argument_count = context_p->literal_count;

  if ((context_p->status_flags & PARSER_IS_PROPERTY_GETTER)
      && context_p->argument_count != 0)
  {
    parser_raise_error (context_p, "Property getters must have no arguments.");
  }

  if ((context_p->status_flags & PARSER_IS_PROPERTY_SETTER)
      && context_p->argument_count != 1)
  {
    parser_raise_error (context_p, "Property setters must have one argument.");
  }

#ifdef PARSER_DEBUG
  if (context_p->is_show_opcodes
      && (context_p->status_flags & PARSER_HAS_NON_STRICT_ARG))
  {
    printf ("  Note: legacy (non-strict) argument definition\n\n");
  }
#endif /* PARSER_DEBUG */

  if (context_p->token.type != LEXER_LEFT_BRACE)
  {
    parser_raise_error (context_p, "Expected '{' token.");
  }

  lexer_next_token (context_p);
  parser_parse_statements (context_p);
  compiled_code_p = parser_post_processing (context_p);

#ifdef PARSER_DEBUG
  if (context_p->is_show_opcodes)
  {
    printf ("\n--- Function parsing end ---\n\n");
  }
#endif /* PARSER_DEBUG */

  parser_cbc_stream_free (&context_p->byte_code);
  parser_free_literals (context_p, &context_p->literal_pool);

  /* Restore private part of the context. */

  PARSER_ASSERT (context_p->last_cbc_opcode == PARSER_CBC_UNAVAILABLE);

  context_p->status_flags = saved_context.status_flags;
  context_p->stack_depth = saved_context.stack_depth;
  context_p->stack_limit = saved_context.stack_limit;
  context_p->last_context_p = saved_context.prev_context_p;
  context_p->last_statement = saved_context.last_statement;

  context_p->argument_count = saved_context.argument_count;
  context_p->register_count = saved_context.register_count;
  context_p->literal_count = saved_context.literal_count;

  context_p->byte_code = saved_context.byte_code;
  context_p->byte_code_size = saved_context.byte_code_size;
  context_p->literal_pool.data = saved_context.literal_pool_data;

  return compiled_code_p;
} /* parser_parse_function */

/**
 * Raise a parse error
 */
void
parser_raise_error (parser_context *context_p, /**< context */
                    const char *error_str_p) /**< error message */
{
  parser_saved_context *saved_context_p = context_p->last_context_p;

  while (saved_context_p != NULL)
  {
    parser_cbc_stream_free (&saved_context_p->byte_code);

    /* First the current literal pool is freed, and then it is replaced
     * by the literal pool coming from the saved context. Since literals
     * are not used anymore, this is a valid replacement. The last pool
     * is freed by parser_parse_script. */

    parser_free_literals (context_p, &context_p->literal_pool);
    context_p->literal_pool.data = saved_context_p->literal_pool_data;

    if (saved_context_p->last_statement.current_p != NULL)
    {
      parser_free_jumps (context_p, saved_context_p->last_statement);
    }

    saved_context_p = saved_context_p->prev_context_p;
  }

  context_p->error_str_p = error_str_p;
  PARSER_THROW (context_p->try_buffer);
  /* Should never been reached. */
  PARSER_ASSERT (0);
} /* parser_raise_error */
