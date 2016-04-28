#include <assert.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/if.h>

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

// "early" linux/can.h -- missing defines in at least kernel version 3.2
#ifndef CAN_MTU
 #define CAN_MTU         (sizeof(struct can_frame))
#endif
#ifndef CAN_MAX_DLEN
 #define CAN_MAX_DLEN 8
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h> 
#include <caml/threads.h> 
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#include <posix-time/posix-time.h>

static value eunix;

CAMLprim value can_initialize(void) {
  CAMLparam0();
  eunix = caml_hash_variant("EUnix");
  CAMLreturn (Val_unit);
}

CAMLprim value can_open(value ifname) {
  CAMLparam1(ifname);
  CAMLlocal2(result, perrno);

  int fd;
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

  if (0 > (fd = socket(PF_CAN, SOCK_RAW, CAN_RAW))) {
    lerrno = errno;
    goto ERROR;
  }

  if (-1 == ioctl(fd, SIOCGIFINDEX, &ifr)) {
    lerrno = errno;
    goto ERROR;
  }

  memset(&addr, 0, sizeof(addr));
  addr.can_family = AF_CAN;
  addr.can_ifindex = ifr.ifr_ifindex;
  if (0 > bind(fd, (struct sockaddr*)&addr, sizeof(addr))) {
    lerrno = errno;
    goto ERROR;
  }

  const int timestamp_on = 1;
  if (0 > setsockopt(fd, SOL_SOCKET, SO_TIMESTAMPNS, &timestamp_on, sizeof(timestamp_on))) {
    lerrno = errno;
    goto ERROR;
  }

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
  CAMLreturn(result);
}

static int err_flag_table[9] = {
  CAN_ERR_TX_TIMEOUT,
  CAN_ERR_LOSTARB,
  CAN_ERR_CRTL,
  CAN_ERR_PROT,
  CAN_ERR_TRX,
  CAN_ERR_ACK,
  CAN_ERR_BUSOFF,
  CAN_ERR_BUSERROR,
  CAN_ERR_RESTARTED
};

CAMLprim value can_error_flags(value socket, value eflags) {
  CAMLparam2(socket, eflags);
  CAMLlocal2(result, perrno);

  can_err_mask_t err_mask;
  int fd, rc;

  fd = Int_val(socket);
  err_mask = convert_flag_list(eflags, err_flag_table);

  caml_release_runtime_system();
  rc = setsockopt(fd, SOL_CAN_RAW, CAN_RAW_ERR_FILTER, &err_mask, sizeof(err_mask));
  caml_acquire_runtime_system();

  if (-1 == rc) {
    goto ERROR;
  }

  result = caml_alloc(1, 0); // Result.Ok
  Store_field(result, 0, socket);
  goto END;

ERROR:
  perrno = caml_alloc(2, 0);
  Store_field(perrno, 0, eunix); // `EUnix
  Store_field(perrno, 1, unix_error_of_code(errno));

  result = caml_alloc(1, 1); // Result.Error
  Store_field(result, 0, perrno);

END:
  CAMLreturn(result);
}

CAMLprim value can_receive_filter(value socket, value flist) {
  CAMLparam2(socket, flist);
  CAMLlocal4(head, tail, result, perrno);

  size_t i, j, size;
  struct can_filter * rfilter;
  int rc, fd;

  fd = Int_val(socket);

  // compute List.length
  i = 0;
  tail = flist;
  while (Val_emptylist != tail) {
    i++;
    tail = Field(tail, 1);
  }
  size = sizeof(struct can_filter) * i;

  // prepare filter
  if (0 == size) {
    // no filter, no messages
    rfilter = NULL;
  } else {
    rfilter = alloca(size);
    j = 0;
    tail = flist;
    while (Val_emptylist != tail) {
      head = Field(tail, 0);
      rfilter[j].can_id = Int32_val(Field(head, 0));
      rfilter[j].can_mask = Int32_val(Field(head, 1));
      j++;
      tail = Field(tail, 1);
    }
  }

  caml_release_runtime_system();
  rc = setsockopt(fd, SOL_CAN_RAW, CAN_RAW_FILTER, rfilter, size);
  caml_acquire_runtime_system();
  
  if (-1 == rc) {
    goto ERROR;
  }

  result = caml_alloc(1, 0); // Result.Ok
  Store_field(result, 0, socket);
  goto END;

ERROR:
  perrno = caml_alloc(2, 0);
  Store_field(perrno, 0, eunix); // `EUnix
  Store_field(perrno, 1, unix_error_of_code(errno));

  result = caml_alloc(1, 1); // Result.Error
  Store_field(result, 0, perrno);

END:
  CAMLreturn(result);
}

CAMLprim value can_receive(value socket) {
  CAMLparam1(socket);
  CAMLlocal5(result, perrno, frame, id, data);
  CAMLlocal3(timestamp, sec, nsec);
  struct can_frame cframe;
  ssize_t len;
  int fd;

  struct msghdr msg;
  struct iovec iov;
  char ctrlmsg[CMSG_SPACE(sizeof(struct timespec))];
  struct timespec t;
  struct cmsghdr *cmsg;

  memset(&msg, 0, sizeof(msg));
  memset(&iov, 0, sizeof(iov));
  fd = Int_val(socket);

  caml_release_runtime_system();

  iov.iov_base = &cframe;
  msg.msg_name = NULL;
  msg.msg_iov = &iov;
  msg.msg_iovlen = 1;
  msg.msg_control = &ctrlmsg;

  iov.iov_len = sizeof(cframe);
  msg.msg_controllen = sizeof(ctrlmsg);  
  msg.msg_flags = 0;

  t.tv_sec = 0;
  t.tv_nsec = 0;

  len = recvmsg(fd, &msg, 0);

  if (CAN_MTU == len) {
    cmsg = CMSG_FIRSTHDR(&msg);
    if (cmsg && SOL_SOCKET == cmsg->cmsg_level) {
      if (SCM_TIMESTAMPNS == cmsg->cmsg_type) {
        memcpy(&t, CMSG_DATA(cmsg), sizeof(t));
      }
    }
  }

  caml_acquire_runtime_system();

  if (CAN_MTU != len) {
    perrno = caml_alloc(2, 0);
    Store_field(perrno, 0, eunix); // `EUnix
    Store_field(perrno, 1, unix_error_of_code(errno));
    result = caml_alloc(1, 1); // Result.Error
    Store_field(result, 0, perrno);
  } else {
    assert(CAN_MAX_DLEN >= cframe.can_dlc);

    id = caml_copy_int32(cframe.can_id);

    data = caml_alloc_string(cframe.can_dlc); // payload
    memcpy(String_val(data), (void *) & cframe.data, cframe.can_dlc);

    frame = caml_alloc(3, 0); // Frame.t
    Store_field(frame, 0, id);
    Store_field(frame, 1, data);
    Store_field(frame, 2, val_timespec(t));

    result = caml_alloc(1, 0); // Result.Ok
    Store_field(result, 0, frame);
  }

  CAMLreturn(result);
}

value can_send(value socket, value frame) {
  CAMLparam2(socket, frame);
  CAMLlocal3(result, data, perrno);
  size_t dlc;
  ssize_t len;
  struct can_frame buffer;
  int fd;

  fd = Int_val(socket);
  data = Field(frame, 1);
  dlc = caml_string_length(data);

  assert(dlc <= CAN_MAX_DLEN);

  memset(&buffer, 0, sizeof(buffer));
  memcpy((void *) & buffer.data, String_val(data), dlc);
  buffer.can_id = Int32_val(Field(frame, 0));
  buffer.can_dlc = dlc;

  caml_release_runtime_system();
  len = write(fd, &buffer, sizeof(buffer));
  caml_acquire_runtime_system();

  if (-1 == len) {
    perrno = caml_alloc(2, 0);
    Store_field(perrno, 0, eunix); // `EUnix
    Store_field(perrno, 1, unix_error_of_code(errno));

    result = caml_alloc(1, 1); // Result.Error
    Store_field(result, 0, perrno);
  } else {
    result = caml_alloc(1, 0); // Result.Ok
    Store_field(result, 0, Val_int(len));
  }

  CAMLreturn(result);
}

