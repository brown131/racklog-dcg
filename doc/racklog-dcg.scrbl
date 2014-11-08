#lang scribble/manual
@;;;; Racklog-DCG
@;;;;
@;;;; racklog-dcg - Documentation module for the project
@;;;;
@;;;; Copyright (c) Scott Brown 2014, All rights reserved.
@;;;;
@;;;; This file is part of Racqlog/DCG
@;;;;
@;;;; Racquel is free software: you can redistribute it and/or modify
@;;;; it under the terms of the GNU General Public License as published by
@;;;; the Free Software Foundation, either version 3 of the License, or
@;;;; (at your option) any later version.
@;;;;
@;;;; This program is distributed in the hope that it will be useful,
@;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
@;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
@;;;; GNU General Public License for more details.
@;;;;
@;;;; You should have received a copy of the GNU General Public License
@;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

@(require racklog-dcg
          scribble/manual scribble/eval scribble/bnf
          (for-label racket)
          (for-syntax racket/base racket/class racket/serialize))

@title{Racklog-DCG: A Definite Clause Grammar notation for Racklog}
 
Racklog-DCG is an extension of Racklog, adding additional primitives for defining a Definate Clause
Grammar.


