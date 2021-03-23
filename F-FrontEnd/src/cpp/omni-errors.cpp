#include "omni_errors.h"
#include <sstream>
#include <exception>

void on_fatal_error(const char* msg, const char *fileName,
        const size_t lineNumber) {
    std::ostringstream stream;
    stream << fileName << ":" << lineNumber << "  FATAL ERROR";
    if (msg) {
        stream << ": " << msg;
    }
    std::string errMsg = stream.str();
    throw std::runtime_error(errMsg);
}