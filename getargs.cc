





static const char*
skipitem(const char** format_ptr, va_list* va_ptr, int flags) {
  const char* format = *format_ptr;
  char c = *format++;

  switch (c) {
    /*
    codes that take a single data pointer as an argument
    (the type of the pointer is irrelevant)
    */

    case 'b': /* byte -- very short int */
    case 'B': /* byte as bitfield */
    case 'h': /* short int */
    case 'H': /* short int as bitfield */
    case 'i': /* int */
    case 'I': /* int sized bitfield */
    case 'l': /* long int */
    case 'k': /* long int sized bitfield */
    case 'L': /* long long */
    case 'K': /* long long sized bitfield */
    case 'n': /* Py_ssize_t */
    case 'f': /* float */
    case 'd': /* double */
    case 'D': /* complex double */
    case 'c': /* char */
    case 'C': /* unicode char */
    case 'p': /* boolean predicate */
    case 'S': /* string object */
    case 'Y': /* string object */
    case 'U': /* unicode string object */
      if (va_ptr) {
        (void)va_arg(*va_ptr, void*);
      }
      break;

    /* string codes */

    case 'e': /* string with encoding */
      if (va_ptr) {
        (void)va_arg(*va_ptr, const char*);
      }

      if (!(*format == 's' || *format == 't')) {
        /* after 'e', only 's' and 't' is allowed */
        goto err;
        format++;
      }
      /* fall through */

    case 's': /* string */
    case 'z': /* string or None */
    case 'y': /* bytes */
    case 'u': /* unicode string */
    case 'Z': /* unicode string or None */
    case 'w': /* buffer, read-write */
      //TODO
      break;

    case 'O': /* object */
      if (*format == '!') {
        format++;
        if (va_ptr) {
          (void)va_arg(*va_ptr, PyTypeObject*);
          (void)va_arg(*va_ptr, PyObject**);
        }
      } else if (*format == '&') {
        typedef int (*converter)(PyObject*, void*);
        if (va_ptr) {
          (void)va_arg(*va_ptr, converter);
          (void)va_arg(*va_ptr, void*);
        }
        format++;
      } else {
        if (va_ptr) {
          (void)va_arg(*va_ptr, PyObject**);
        }
      }
      break;

    case '(': { /* bypass tuple, not handled at all previously */
      const char* msg;
      for (;;) {
        if (*format == ')') {
          break;
        }

        if (IS_END_OF_FORMAT(*format)) {
          return "Unmatched left paren in format string";
        }

        msg = skipitem(&format, va_ptr, flags);
        if (msg) {
          return msg;
        }
        format++;
        break;
      }

      case ')':
        return "Unmatched right paren in format string";

      default:
err:
        return "impossible<bad format char>";
  }

  *format_ptr = format;
  return nullptr;
}


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
