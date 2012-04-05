/*
 * This file is part of Baardskeerder.
 *
 * Copyright (C) 2011 Incubaid BVBA
 *
 * Baardskeerder is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Baardskeerder is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Baardskeerder.  If not, see <http://www.gnu.org/licenses/>.
 */

#define _XOPEN_SOURCE 600
#define _FILE_OFFSET_BITS 64
#define _GNU_SOURCE

#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>

#include <linux/falloc.h>
#include <linux/fs.h>
#include <linux/fiemap.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>

void _bs_posix_pread_into_exactly(value fd, value buf, value count,
        value offset) {

        CAMLparam4(fd, buf, count, offset);

        int c_fd = 0;
        size_t c_count = 0;
        off_t c_offset = 0;

        size_t read = 0;

        ssize_t r = 0;

        c_fd = Int_val(fd);
        c_count = Long_val(count);
        c_offset = Long_val(offset);

        while(read < c_count) {
                r = pread(c_fd, &Byte(buf, read), c_count - read,
                        c_offset + read);

                if(r == 0) {
                        caml_raise_end_of_file();
                }

                if(r < 0) {
                        uerror("pread", Nothing);
                }

                read += r;
        }

        CAMLreturn0;
}

void _bs_posix_pwrite_exactly(value fd, value buf, value count,
        value offset) {

        CAMLparam4(fd, buf, count, offset);

        int c_fd = 0;
        size_t c_count = 0;
        off_t c_offset = 0;

        size_t written = 0;

        ssize_t r = 0;

        c_fd = Int_val(fd);
        c_count = Long_val(count);
        c_offset = Long_val(offset);

        while(written < c_count) {
                r = pwrite(c_fd, &Byte(buf, written), c_count - written,
                        c_offset + written);

                if(r < 0) {
                        uerror("pwrite", Nothing);
                }

                written += r;
        }

        CAMLreturn0;
}

void _bs_posix_fsync(value fd) {
        int ret = 0;

        CAMLparam1(fd);

        enter_blocking_section();
          ret = fsync(Int_val(fd));
        leave_blocking_section();

        if(ret < 0) {
                uerror("fsync", Nothing);
        }

        CAMLreturn0;
}

void _bs_posix_fdatasync(value fd) {
        int ret = 0;

        CAMLparam1(fd);

        enter_blocking_section();
          ret = fdatasync(Int_val(fd));
        leave_blocking_section();

        if(ret < 0) {
                uerror("fdatasync", Nothing);
        }

        CAMLreturn0;
}


CAMLprim value _bs_posix_fallocate_FALLOC_FL_KEEP_SIZE(value unit) {
        CAMLparam1(unit);
        CAMLlocal1(ret);

        ret = Val_int(FALLOC_FL_KEEP_SIZE);

        CAMLreturn(ret);
}

CAMLprim value _bs_posix_fallocate_FALLOC_FL_PUNCH_HOLE(value unit) {
        CAMLparam1(unit);
        CAMLlocal1(ret);

        ret = Val_int(FALLOC_FL_PUNCH_HOLE);

        CAMLreturn(ret);
}

void _bs_posix_fallocate(value fd, value mode, value offset,
        value len) {
        int ret = 0;

        int c_fd = -1;
        int c_mode = 0;
        off_t c_offset = 0;
        off_t c_len = 0;

        CAMLparam4(fd, mode, offset, len);

        c_fd = Int_val(fd);
        c_mode = Int_val(mode);
        c_offset = Long_val(offset);
        c_len = Long_val(len);

        ret = fallocate(c_fd, c_mode, c_offset, c_len);

        if(ret < 0) {
                uerror("fallocate", Nothing);
        }

        CAMLreturn0;
}


void _bs_posix_fadvise(value fd, value offset, value len, value advice) {
        int ret = 0;

        int c_fd = -1;
        off_t c_offset = 0;
        off_t c_len = 0;
        int c_advice = 0;

        CAMLparam4(fd, offset, len, advice);

        c_fd = Int_val(fd);
        c_offset = Long_val(offset);
        c_len = Long_val(len);

        switch(Int_val(advice)) {
                case 0:
                        c_advice = POSIX_FADV_NORMAL;
                        break;
                case 1:
                        c_advice = POSIX_FADV_SEQUENTIAL;
                        break;
                case 2:
                        c_advice = POSIX_FADV_RANDOM;
                        break;
                case 3:
                        c_advice = POSIX_FADV_NOREUSE;
                        break;
                case 4:
                        c_advice = POSIX_FADV_WILLNEED;
                        break;
                case 5:
                        c_advice = POSIX_FADV_DONTNEED;
                        break;
                default:
                        caml_invalid_argument("advice");
                        break;
        }

        enter_blocking_section();
          ret = posix_fadvise(c_fd, c_offset, c_len, c_advice);
        leave_blocking_section();

        if(ret != 0) {
                uerror("posix_fadvise", Nothing);
        }

        CAMLreturn0;
}


CAMLprim value _bs_posix_fstat_blksize(value fd) {
        int ret = 0;
        struct stat buf;

        memset(&buf, 0, sizeof(struct stat));

        ret = fstat(Int_val(fd), &buf);

        if(ret < 0) {
                uerror("fstat", Nothing);
        }

        if(buf.st_blksize > Max_long) {
                unix_error(EOVERFLOW, "fstat", Nothing);
        }

        return Val_int(buf.st_blksize);
}


CAMLprim value _bs_posix_ioctl_fiemap(value fd) {
        int c_fd = -1, i = 0;
        unsigned int size = 0;
        struct fiemap fiemap, *fiemap2 = NULL;
        struct fiemap_extent *extent = NULL;

        CAMLparam1(fd);
        CAMLlocal3(ret, item, cell);

        c_fd = Int_val(fd);

        memset(&fiemap, 0, sizeof(struct fiemap));

        fiemap.fm_start = 0;
        fiemap.fm_length = ~0;
        fiemap.fm_flags = FIEMAP_FLAG_SYNC;
        fiemap.fm_extent_count = 0;
        fiemap.fm_mapped_extents = 0;

        if(ioctl(c_fd, FS_IOC_FIEMAP, &fiemap) < 0) {
                uerror("ioctl", Nothing);
        }

        size = sizeof(struct fiemap) +
                sizeof(struct fiemap_extent) * (fiemap.fm_mapped_extents);
        fiemap2 = (struct fiemap *)malloc(size);
        if(fiemap2 == NULL) {
                uerror("malloc", Nothing);
        }

        memset(fiemap2, 0, size);
        fiemap2->fm_start = 0;
        fiemap2->fm_length = ~0;
        fiemap2->fm_flags = FIEMAP_FLAG_SYNC;
        fiemap2->fm_extent_count = fiemap.fm_mapped_extents;
        fiemap2->fm_mapped_extents = 0;

        if(ioctl(c_fd, FS_IOC_FIEMAP, fiemap2) < 0) {
                free(fiemap2);

                uerror("ioctl", Nothing);
        }

        ret = Val_emptylist;

        for(i = fiemap2->fm_mapped_extents - 1; i >= 0; i--) {
                item = caml_alloc_tuple(4);
                extent = &fiemap2->fm_extents[i];

                Store_field(item, 0, caml_copy_int64(extent->fe_logical));
                Store_field(item, 1, caml_copy_int64(extent->fe_physical));
                Store_field(item, 2, caml_copy_int64(extent->fe_length));
                Store_field(item, 3, caml_copy_int32(extent->fe_flags));

                cell = caml_alloc_small(2, Tag_cons);
                Store_field(cell, 0, item);
                Store_field(cell, 1, ret);

                ret = cell;
        }

        free(fiemap2);

        CAMLreturn(ret);
}


/* Lwt bindings */
#include <lwt_unix.h>

/* TODO
 * Check int type sizes. The current code does what Lwt does upstream
 * (in 2.3.2), but it feels like int and long, signed and unsigned are mixed
 * and matched all around, whilst POSIX defines size_t and off_t.
 */

CAMLprim value lwt_unix_ext_pread(value val_fd, value val_buf, value val_ofs, value val_len, value val_offset)
{
  int ret;
  ret = pread(Int_val(val_fd), &Byte(String_val(val_buf), Long_val(val_ofs)), Long_val(val_len), Long_val(val_offset));
  if (ret == -1) uerror("pread", Nothing);
  /* Lwt uses Val_int here, but I'm pretty sure this must be Val_long,
   * similar to lwt_unix_ext_pread_result */
  return Val_long(ret);
}

struct job_pread {
  struct lwt_unix_job job;
  int fd;
  char *buffer;
  int length;
  int offset;
  int result;
  int error_code;
};

#define Job_pread_val(v) *(struct job_pread**)Data_custom_val(v)

static void worker_pread(struct job_pread *job)
{
  job->result = pread(job->fd, job->buffer, job->length, job->offset);
  job->error_code = errno;
}

CAMLprim value lwt_unix_ext_pread_job(value val_fd, value val_length, value val_offset)
{
  struct job_pread *job = lwt_unix_new(struct job_pread);
  long length = Long_val(val_length);
  job->job.worker = (lwt_unix_job_worker)worker_pread;
  job->fd = Int_val(val_fd);
  job->buffer = (char*)lwt_unix_malloc(length);
  job->length = length;
  job->offset = Long_val(val_offset);
  return lwt_unix_alloc_job(&(job->job));
}

CAMLprim value lwt_unix_ext_pread_result(value val_job, value val_string, value val_offset)
{
  struct job_pread *job = Job_pread_val(val_job);
  int result = job->result;
  if (result < 0) unix_error(job->error_code, "pread", Nothing);
  memcpy(String_val(val_string) + Long_val(val_offset), job->buffer, result);
  return Val_long(result);
}

CAMLprim value lwt_unix_ext_pread_free(value val_job)
{
  struct job_pread *job = Job_pread_val(val_job);
  free(job->buffer);
  lwt_unix_free_job(&job->job);
  return Val_unit;
}
