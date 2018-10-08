#include "ringbuf.h"

static ringbuf_t* tracing_buffer = NULL;
static unsigned char* tracing_buffer_space = NULL;
static ringbuf_worker_t* tracing_worker = NULL;
static int tracing_buffer_enabled = 1;
static const size_t tracing_buffer_slots = 2048;
static int caml_tracing_failed = 0;

void caml_tracing_initialise_buffer (void)
{
  int result;

  if (tracing_buffer_enabled) {
    size_t ringbuf_obj_size;
    size_t space_size;

    ringbuf_get_sizes(2, &ringbuf_obj_size, NULL);

    tracing_buffer = (ringbuf_t*) malloc(ringbuf_obj_size);
    if (tracing_buffer == NULL) {
      caml_fatal_error ("Could not allocate tracing buffer");
    }

    space_size = tracing_buffer_slots * sizeof(uintnat);
    tracing_buffer_space = (uintnat*) malloc(space_size);
    if (tracing_buffer_space == NULL) {
      caml_fatal_error ("Could not allocate tracing buffer space");
    }

    result = ringbuf_setup (tracing_buffer, 2, space_size);
    if (result != 0) {
      caml_fatal_error ("Could not set up tracing buffer");
    }

    tracing_worker = ringbuf_register(tracing_buffer, 0);
    if (tracing_worker == NULL) {
      caml_fatal_error ("Could not register producer for tracing buffer");
    }
  }
}

void caml_tracing_write_one_word (uintnat word)
{
  ssize_t offset;
  offset = ringbuf_acquire(tracing_buffer, tracing_worker, sizeof(uintnat));
  if (offset >= 0) {
    tracing_buffer_space[offset] = word0;
    ringbuf_produce(tracing_buffer, tracing_worker);
  }
  else {
    caml_tracing_failed = 1;
  }
}

void caml_tracing_write_two_words (uintnat first, uintnat second)
{
  ssize_t offset;
  offset = ringbuf_acquire(tracing_buffer, tracing_worker,
                           2 * sizeof(uintnat));
  if (offset >= 0) {
    ((uintnat*) (tracing_buffer_space[offset])) = first;
    ((uintnat*) (tracing_buffer_space[offset + sizeof(uintnat)])) = second;
    ringbuf_produce(tracing_buffer, tracing_worker);
  }
  else {
    caml_tracing_failed = 1;
  }
}

void caml_tracing_write_three_words (uintnat first, uintnat second,
                                     uintnat third)
{
  ssize_t offset;
  offset = ringbuf_acquire(tracing_buffer, tracing_worker,
                           3 * sizeof(uintnat));
  if (offset >= 0) {
    ((uintnat*) (tracing_buffer_space[offset])) = first;
    ((uintnat*) (tracing_buffer_space[offset + sizeof(uintnat)])) = second;
    ((uintnat*) (tracing_buffer_space[offset + 2*sizeof(uintnat)])) = third;
    ringbuf_produce(tracing_buffer, tracing_worker);
  }
  else {
    caml_tracing_failed = 1;
  }
}

CAMLprim value caml_tracing_has_failed (value v_unit)
{
  return Val_int (caml_tracing_failed);
}

static const uint8_t protocol_version = 0;

typedef enum {
  ENTERING_FUNCTION = 0,
  LEAVING_FUNCTION,
  PROMOTION,
  SWEPT,
} protocol_message_type;

static void make_header (protocol_message_type msg_type)
{
  return ((uintnat) protocol_version) | (((uintnat) msg_type) << 8);
}

static void write_with_header0 (protocol_message_type msg_type)
{
  caml_tracing_write_one_word (make_header (msg_type));
}

static void write_with_header1 (protocol_message_type msg_type, uintnat word)
{
  caml_tracing_write_two_words (make_header (msg_type), word);
}

static void write_with_header2 (protocol_message_type msg_type, uintnat first,
                                uintnat second)
{
  caml_tracing_write_three_words (make_header (msg_type), first, second);
}

void caml_tracing_entering_function (uintnat pc)
{
  write_with_header1 (ENTERING_FUNCTION, pc);
}

void caml_tracing_leaving_function (void)
{
  write_with_header0 (LEAVING_FUNCTION);
}

void caml_tracing_promotion (uintnat old_block, uintnat new_block)
{
  write_with_header2 (PROMOTION, old_block, new_block);
}

void caml_tracing_swept (uintnat block);
{
  write_with_header1 (SWEPT, block);
}
