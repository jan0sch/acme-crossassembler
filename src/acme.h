// ACME - a crossassembler for producing 6502/65c02/65816 code.
// Copyright (C) 1998-2014 Marco Baye
// Have a look at "acme.c" for further info
//
// Main definitions
#ifndef acme_H
#define acme_H


#include "config.h"


// Variables
extern const char	*symbollist_filename;
extern const char	*output_filename;
extern const char	*report_filename;
// maximum recursion depth for macro calls and "!source"
extern signed long	macro_recursions_left;
extern signed long	source_recursions_left;


// Prototypes

// tidy up before exiting by saving symbol dump
extern int ACME_finalize(int exit_code);


#endif
