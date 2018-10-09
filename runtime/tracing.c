#include "ringbuf.h"

typedef enum {
  ENTERING_FUNCTION = 0,
  LEAVING_FUNCTION,
  PROMOTION,
  SWEPT,
} protocol_message_type;

static ringbuf_t* tracing_buffer = NULL;
static unsigned char* tracing_buffer_space = NULL;
static ringbuf_worker_t* tracing_worker = NULL;
static int tracing_buffer_enabled = 1;
static const size_t tracing_buffer_slots = 2048;
static int caml_tracing_failed = 0;

static protocol_state consumer_state;

static const uint8_t protocol_version = 0;

static uint8_t protocol_version_of_header (uintnat header)
{
  return (uint8_t) (header & 0xff);
}

static protocol_message_type message_type_of_header (uintnat header)
{
  return (protocol_message_type) (header >> 8);
}

static void consume_entering_function (uintnat pc)
{
  printf("Entering function %p\n", (void*) pc);
  fflush(stdout);
}

static void consume_leaving_function (void)
{
  printf("Leaving function\n");
  fflush(stdout);
}

static void consume_promotion (uintnat old_block, uintnat new_block)
{
  printf("Promotion %p -> %p\n", (void*) old_block, (void*) new_block);
  fflush(stdout);
}

static void consume_swept (uintnat block)
{
  printf("Swept %p\n%!", (void*) block);
  fflush(stdout);
}

void caml_tracing_initialise_buffer (void)
{
  int result;

  if (tracing_buffer_enabled) {
    size_t ringbuf_obj_size;
    size_t space_size;
    pid_t consumer_pid;
    int shm_id;
    int space_shm_id;

    ringbuf_get_sizes(2, &ringbuf_obj_size, NULL);

    shm_id = shmget(IPC_PRIVATE, ringbuf_obj_size, IPC_CREAT);
    if (shm_id < 0) {
      caml_fatal_error ("Could not allocate tracing buffer");
    }

    tracing_buffer = (ringbuf_t*) shmat(shm_id, NULL, 0);
    if (tracing_buffer == (void*) -1) {
      caml_fatal_error ("Could not attach tracing buffer");
    }

    space_size = tracing_buffer_slots * sizeof(uintnat);
    space_shm_id = shmget(IPC_PRIVATE, space_size, IPC_CREAT);
    if (space_shm_id < 0) {
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

    tracing_buffer_space = (uintnat*) shmat(space_shm_id, NULL, 0);
    if (tracing_buffer_space == (void*) -1) {
      caml_fatal_error ("Could not attach tracing buffer space");
    }

    consumer_pid = fork ();
    if (consumer_pid < 0) {
      caml_fatal_error ("Could not fork tracing consumer process");
    } else if (consumer_pid == 0) {
      while (1) {
        size_t avail;
        size_t offset;
        size_t words_required;
        ringbuf_t* consumer_tracing_buffer;
        uintnat* consumer_tracing_buffer_space;
        ringbuf_worker_t* consumer_worker = NULL;

        consumer_tracing_buffer = (ringbuf_t*) shmat(shm_id, NULL, 0);
        if (consumer_tracing_buffer == (void*) -1) {
          caml_fatal_error ("Could not attach consumer tracing buffer");
        }

        consumer_tracing_buffer_space =
          (uintnat*) shmat(space_shm_id, NULL, SHM_RDONLY);
        if (consumer_tracing_buffer_space == (void*) -1) {
          caml_fatal_error ("Could not attach consumer tracing buffer space");
        }

        consumer_worker = ringbuf_register(consumer_tracing_buffer, 0);
        if (consumer_worker == NULL) {
          caml_fatal_error ("Could not register consumer for tracing buffer");
        }

        avail = ringbuf_consume(consumer_tracing_buffer, &offset);
        if (avail >= sizeof(uintnat) && avail % sizeof(uintnat) == 0) {
          uintnat* base = (uintnat*) (consumer_tracing_buffer_space + offset);
          uintnat header = base[0];
          size_t bytes_to_free;

          uint8_t sender_protocol_version = protocol_version_of_header(header);
          if (protocol_version != sender_protocol_version) {
            caml_fatal_error ("Tracing consumer cannot understand protocol");
          }

          protocol_message_type msg_type = message_type_of_header(header);
          switch (msg_type) {
            case ENTERING_FUNCTION:
              words_required = 1;
              break;

            case LEAVING_FUNCTION:
              words_required = 0;
              break;

            case PROMOTION:
              words_required = 2;
              break;

            case SWEPT:
              words_required = 1;
              break;

            default:
              caml_fatal_error("Tracing consumer cannot understand header");
          }

          bytes_to_free = (1 + words_required) * sizeof(uintnat);

          while (words_required >= 0) {
            avail = ringbuf_consume(consumer_tracing_buffer, &offset);
            if (avail >= sizeof(uintnat) && avail % sizeof(uintnat) == 0) {
              words_required -= avail / sizeof(uintnat);
            }
          }

          switch (msg_type) {
            case ENTERING_FUNCTION:
              consume_entering_function(base[1]);
              break;

            case LEAVING_FUNCTION:
              consume_leaving_function();
              break;

            case PROMOTION:
              consume_promotion(base[1], base[2]);
              break;

            case SWEPT:
              consume_swept(base[1]);
              break;

            default:
              assert (0);
          }

          ringbuf_release(consumer_tracing_buffer, bytes_to_free);
        }
      }
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

void caml_tracing_promotion (value old_block, value new_block)
{
  write_with_header2 (PROMOTION, (uintnat) old_block, (uintnat) new_block);
}

void caml_tracing_swept (value block)
{
  write_with_header1 (SWEPT, (uintnat) block);
}
