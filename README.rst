python-x.el:  python.el extras for interactive evaluation
=========================================================

``python-x`` extends the built-in ``python-mode`` (F. Gallina's) with
several additional functions and behaviors inspired by ess-mode_ which
are targeted to interactive code evaluation with an inferior Python
process.

- Additional evaluation and stepping by lines, paragraphs, sections and
  folds.
- Evaluation feedback using volatile-highlights_.
- Navigation by sections/folds_ (like notebook/MATLAB code "cells"),
  with native support for expand-region_.
- Improved exception handling.
- Improved ElDoc/Help behavior.

Use ``python-x-setup`` in your emacs startup:

.. code:: elisp

   (python-x-setup)

In any ``python-mode`` buffer use ``C-c C-p`` to start a new
interpreter, then use ``C-c C-c`` to evaluate the current section. The
default section delimiter is ``# ---`` but can be changed via
``python-section-delimiter`` to be similar to Spyder/Pyzo:

.. code:: elisp

  (setq python-section-delimiter "##")

See the ``python-x`` customization group for additional settings.

This package is fully documented in the source and maintained through
MELPA:

https://melpa.org/#/python-x

.. _volatile-highlights: http://melpa.org/#/volatile-highlights
.. _expand-region: http://melpa.org/#/expand-region
.. _folds: http://melpa.org/#/folding
.. _ess-mode: http://ess.r-project.org/
