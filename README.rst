python-x.el:  python.el extras for interactive evaluation
=========================================================

``python-x`` extends the built-in ``python-mode`` (F. Gallina's) with several
additional functions and behaviors inspired by ess-mode_ which are targeted to
interactive code evaluation with an inferior Python process.

- Additional evaluation and stepping by lines, paragraphs, sections and folds.
- Evaluation feedback using volatile-highlights_.
- Navigation by sections/folds_ (like notebook/MATLAB code "cells"), with
  native support for expand-region_.
- Improved exception handling.
- Improved ElDoc/Help behavior.

This package is fully documented in the source and maintained through MELPA:

http://melpa.org/#/python-x

.. _volatile-highlights: http://melpa.org/#/volatile-highlights
.. _expand-region: http://melpa.org/#/expand-region
.. _folds: http://melpa.org/#/folding
.. _ess-mode: http://ess.r-project.org/
