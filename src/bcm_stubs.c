#include <assert.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>

#include <assert.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>

#include <linux/can.h>
#include <linux/can/raw.h>
#include <linux/can/error.h>
#include <linux/can/bcm.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h> 
#include <caml/threads.h> 
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#include <posix-time/posix-time.h>

static value eunix;

CAMLprim value bcm_initialize(void) {
  CAMLparam0();
  eunix = caml_hash_variant("EUnix");
  CAMLreturn (Val_unit);
}

CAMLprim value bcm_open(value ifname) {
  CAMLparam1(ifname);
  CAMLlocal2(result, perrno);

  int fd = -1;
  struct ifreq ifr;
  struct sockaddr_can addr;
  size_t ifnamelen;
  int lerrno = 0;

  ifnamelen = caml_string_length(ifname);
  if ((ifnamelen + 1) > sizeof(ifr.ifr_name)) {
    lerrno = ENAMETOOLONG;
    goto ERROR_CLEAN;
  }

  // copy string and put terminating \0 at it's end
  memcpy(ifr.ifr_name, String_val(ifname), ifnamelen);
  ifr.ifr_name[ifnamelen] = '\0';

  caml_release_runtime_system();

  if (0 > (fd = socket(PF_CAN, SOCK_DGRAM, CAN_BCM))) {
    lerrno = errno;
    goto ERROR;
  }

  if (-1 == ioctl(fd, SIOCGIFINDEX, &ifr)) {
    lerrno = errno;
    goto ERROR;
  }
  addr.can_family = AF_CAN;
  addr.can_ifindex = ifr.ifr_ifindex;

  if (0 > connect(fd, (struct sockaddr*)&addr, sizeof(addr))) {
    lerrno = errno;
    goto ERROR;
  }
  assert(-1 != fd);

  caml_acquire_runtime_system();

  result = caml_alloc(1, 0); // Result.Ok
  Store_field(result, 0, Val_int(fd));
  goto END;

ERROR:
  close(fd);
  caml_acquire_runtime_system();

ERROR_CLEAN:
  perrno = caml_alloc(2, 0);
  Store_field(perrno, 0, eunix); // `EUnix
  Store_field(perrno, 1, unix_error_of_code(lerrno));

  result = caml_alloc(1, 1); // Result.Error
  Store_field(result, 0, perrno);

END:
  CAMLreturn(Val_int(fd));
}

static int bcm_opcode_table[12] = {
  TX_SETUP,
  TX_DELETE,
  TX_READ,
  TX_SEND,
  TX_STATUS,
  TX_EXPIRED,
  RX_SETUP,
  RX_DELETE,
  RX_READ,
  RX_STATUS,
  RX_TIMEOUT,
  RX_CHANGED
};

static int bcm_flag_table[11] = {
  SETTIMER,
  STARTTIMER,
  TX_COUNTEVT,
  TX_ANNOUNCE,
  TX_CP_CAN_ID,
  RX_FILTER_ID,
  RX_CHECK_DLC,
  RX_NO_AUTOTIMER,
  RX_ANNOUNCE_RESUME,
  TX_RESET_MULTI_IDX,
  RX_RTR_FRAME
};

CAMLprim value bcm_write(value socket, value opcodes, value flags, value options, value frames) {
  CAMLparam5(socket, opcodes, flags, options, frames);
  CAMLlocal5(head, tail, data, result, perrno);

  int fd;
  struct bcm_msg_head * msg;
  uint32_t nframes, i;
  size_t msg_size, rc, dlc;

  // compute List.length of frames list
  for (nframes = 0, tail = frames; Val_emptylist != tail; nframes++, tail = Field(tail, 1)) {}

  msg_size = sizeof(struct bcm_msg_head) + nframes * sizeof(struct can_frame);
  msg = alloca(msg_size);

  fd = Int_val(socket);

  msg->opcode = convert_flag_list(opcodes, bcm_opcode_table);
  msg->flags = convert_flag_list(flags, bcm_flag_table);
  msg->count = Int_val(Field(options, 0));
  msg->ival1 = timeval_val(Field(options, 1));
  msg->ival2 = timeval_val(Field(options, 2));
  msg->can_id = Int_val(Field(options, 3));
  msg->nframes = nframes;

  // copy the can-frames from the frames list to bcm_msg_head
  for (i = 0, tail = frames; Val_emptylist != tail; i++, tail = Field(tail, 1)) {
    assert(i < nframes);
    head = Field(tail, 0);

    msg->frames[i].can_id = Int_val(Field(head, 0));

    data = Field(head, 1);
    dlc = caml_string_length(data);
    assert(dlc <= CAN_MAX_DLEN);
    msg->frames[i].can_dlc = dlc;

    memcpy(& msg->frames[i].data, String_val(data), dlc);
  }

  caml_release_runtime_system();
  rc = write(fd, msg, msg_size);
  caml_acquire_runtime_system();

  if (rc != msg_size) {
    perrno = caml_alloc(2, 0);
    Store_field(perrno, 0, eunix); // `EUnix
    Store_field(perrno, 1, unix_error_of_code(errno));

    result = caml_alloc(1, 1); // Result.Error
    Store_field(result, 0, perrno);
  } else {
    result = caml_alloc(1, 0); // Result.Ok
    Store_field(result, 0, Val_int(fd));
  }

  CAMLreturn(result);
}

