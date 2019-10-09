#include <sys/wait.h>
#include <unistd.h>

#include <sicstus/sicstus.h>

void unix_raise_exception(char const *msg) {
  SP_term_ref m = SP_new_term_ref();
  SP_put_string(m, msg);
  SP_raise_exception(m);
}

SP_term_ref pl_fork() {
  SP_term_ref r = SP_new_term_ref();

  pid_t pid = fork();
  if (pid == 0)
    SP_put_string(r, "child");
  else if (pid > 0) {
    if (SP_put_integer(r, pid) == 0)
      unix_raise_exception("fork/1: returned PID is not an integer");
  }
  else
    unix_raise_exception("fork/1: error in system call");

  return r;
}

void pl_reap_zombies() {
  while (1) {
   int r = waitpid(0, NULL, WNOHANG);
   if (r == 0 || r == -1)
     return;
  }
}

SP_term_ref pl_wait(long *pid) {
  SP_term_ref r = SP_new_term_ref();

  int stat;
  *pid = wait(&stat);
  if (*pid == -1) {
    unix_raise_exception("wait/2: error in system call (no child?)");
    return r;
  }

  SP_term_ref functor = SP_new_term_ref();
  SP_term_ref rStat = SP_new_term_ref();

  if (WIFEXITED(stat)) {
    if (SP_put_integer(rStat, WEXITSTATUS(stat)) == 0)
      unix_raise_exception("wait/2: process return code is not an integer");
    else
      SP_cons_functor(r, SP_atom_from_string("exited"), 1, rStat);
  }
  else if (WIFSIGNALED(stat)) {
    if (SP_put_integer(rStat, WTERMSIG(stat)) == 0)
      unix_raise_exception("wait/2: process return code is not an integer");
    else
      SP_cons_functor(r, SP_atom_from_string("signaled"), 1, rStat);
  }
  else if (WIFSTOPPED(stat)) {
    if (SP_put_integer(rStat, WSTOPSIG(stat)) == 0)
      unix_raise_exception("wait/2: process return code is not an integer");
    else
      SP_cons_functor(r, SP_atom_from_string("stopped"), 1, rStat);
  }
  else {
    SP_put_string(r, "unknown");
  }

  return r;
}
