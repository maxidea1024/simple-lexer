

int _PyArg_CheckPositional(const char* name, Py_ssize_t nargs, Py_ssize_t min, Py_ssize_t max) {
  assert(min >= 0);
  assert(min <= max);
  
  if (nargs < min) {
    if (name) {
      //ERROR
    } else {
      //ERROR
    }
    
    return 0;
  }
  
  if (nargs == 0) {
    return 1;
  }
  
  if (nargs > max) {
    if (name) {
      //ERROR
    } else {
      //ERROR
    }
  }

  return 1;
}


static int
_UnpackStack(PyObject* args, Py_ssize_t nargs, const char* name,
              Py_ssize_t min, Py_ssize_t max, va_list vargs) {
  Py_ssize_t i;
  PyObject** o;
  
  if (!_PyArg_CheckPositional(name, nargs, min, max)) {
    return 0;
  }
  
  for (i = 0; i < nargs; ++i) {
    o = va_arg(vargs, PyObject**);
    *o = args[i];
  }
  return 1;
}
