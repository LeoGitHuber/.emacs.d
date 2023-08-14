;;; init-hydra --- Init for Hydra
;;; Commentary:
;;; Code:

(add-to-list 'load-path "~/.emacs.d/site-lisp/hydra")
(require 'hydra)

(defhydra hydra-avy (global-map "M-g" :exit t :hint nil)
  "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_c_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
  ("c" avy-goto-char-timer)
  ("C" avy-goto-char)
  ("w" avy-goto-word-1)
  ("W" avy-goto-word-0)
  ("l" avy-goto-line)
  ("L" avy-goto-end-of-line)
  ("m" avy-move-line)
  ("M" avy-move-region)
  ("k" avy-kill-whole-line)
  ("K" avy-kill-region)
  ("y" avy-copy-line)
  ("Y" avy-copy-region))

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("r" (text-scale-set 0) "reset")
  ("q" nil "quit"))

(defhydra hydra-window (:color pink)
  "
       Split:                   Move:
  ╭──────────────────────╯ ╭──────────────────────╯
     _v_ vertical            _j_ down
     _h_ horizontal          _k_ up
     _V_ even vertical       _J_ swap down
     _H_ even horizontal     _K_ swap up
     _s_ swap                _L_ swap right
  ╰──────────────────────╮ _l_ right
     _D_lt   _d_lt all         _o_nly this
     _B_ur   _b_ur all         _a_ce  this
     _m_inimize              _z_en
     _q_uit                  _f_ullscreen
  "
  ("<left>" windmove-left)
  ("<down>" windmove-down)
  ("<up>" windmove-up)
  ("<right>" windmove-right)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("o" delete-other-windows)
  ("a" ace-swap-window)
  ("D" delete-window :color blue)
  ("B" balance-windows)
  ("s" ace-swap-window)
  ("m" minimize-window)
  ("z" toggle-maximize-buffer)
  ("f" toggle-frame-fullscreen)
  ("v" split-window-right)
  ("h" split-window-below)
  ("V" split-window-right-and-focus)
  ("H" split-window-below-and-focus)
  ("d" kill-buffer-and-window)
  ("b" kill-buffer-and-window)
  ("L" transpose-frame)
  ("K" buf-move-up)
  ("J" buf-move-down)
  ("q" nil :color blue))

(global-set-key (kbd "C-c w") 'hydra-window/body)

(defhydra hydra-eMove ()
  "
 Line^^           char^^              word^^		          Page^^
---------------------------------------------------------------------
 [_j_] next       [_l_] forward       [_F_] Forward         [_v_] up
 [_k_] previous   [_h_] backward      [_B_] Backward        [_V_] down
 "
  ("j" next-line nil)
  ("k" previous-line nil)
  ("v" scroll-uppp nil)
  ("V" scroll-down nil)
  ("l" forward-char nil)
  ("h" backward-char nil)
  ("F" forward-word nil)
  ("B" backward-word nil)
  ("q" nil "quit")
  )

(keymap-global-set "C-M-;" 'hydra-eMove/body)

;; (require 'rect)

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode t)
  									 :color pink
  									 :post (deactivate-mark))
  "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
  ("h" rectangle-backward-char nil)
  ("l" rectangle-forward-char nil)
  ("k" rectangle-previous-line nil)
  ("j" rectangle-next-line nil)
  ("e" hydra-ex-point-mark nil)
  ("n" copy-rectangle-as-kill nil)
  ("d" delete-rectangle nil)
  ("r" (if (region-active-p)
  		   (deactivate-mark)
  		 (rectangle-mark-mode t)) nil)
  ("y" yank-rectangle nil)
  ("u" undo nil)
  ("s" string-rectangle nil)
  ("p" kill-rectangle nil)
  ("o" nil nil))

(keymap-global-set "C-x SPC" 'hydra-rectangle/body)

(provide 'init-hydra)
;;; init-hydra.el ends here.
