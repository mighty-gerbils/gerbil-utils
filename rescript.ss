;; -*- Gerbil -*-
;; ᴸᴀᵀᴇᵡ

(export superscriptize subscriptize upsidedown leftright mathbb smallcaps fbchange)

(import :std/iter :std/misc/hash :std/srfi/13 :std/sugar
        :clan/base :clan/ports :clan/with-id)

(def (make-script-table original translated reverse?: (reverse? #f))
  (def h (hash))
  (for ((x original)
        (y translated))
    (hash-put! h x y)
    (when reverse?
      (hash-put! h y x)))
  h)

(defrules define-script-translation ()
  ((_ name original translated) (define-script-translation name original translated #f))
  ((_ name original translated reverse?)
   (with-id ((table #'name "-table")
             (process-char #'name "-character"))
     (begin
       (def table (make-script-table original translated reverse?: reverse?))
       (def (process-char c)
         (hash-ref/default table c (cut error "Cannot process char" 'process-char c)))
       (def (name s)
         (def t (with-output (o #f) (for (c s) (display (process-char c) o))))
         (if reverse? (string-reverse t) t))))))

(define-script-translation superscriptize
  " 0123456789+-=()abcdefghijklmnoprstuvwxyzABDEGHIJKLMNOPRTUVWαβγδεθιΦφχ"
  " ⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁⱽᵂᵅᵝᵞᵟᵋᶿᶥᶲᵠᵡ")

(define-script-translation subscriptize
  " 0123456789+-=()aehijklmnoprstuvxβγρφχəا"
  " ₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓᵦᵧᵨᵩᵪₔٖ")

(define-script-translation upsidedown
  " zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA0987654321&_?!\"'.,;"
  " zʎxʍʌnʇsɹbdouɯlʞɾıɥɓɟǝpɔqɐZ⅄XMΛ∩⊥SᴚԾԀONW⅂⋊ſIH⅁ℲƎᗡƆ𐐒∀068ㄥ9ގㄣƐᄅ⇂⅋‾¿¡„,˙'؛"
  #t)

(define-script-translation leftright
  " 018!\"'.:-_+=|()[]{}<>/\\´`ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  " 018!\"'.:-_+=|)(][}{><\\/`´ᗅᗺƆᗡƎꟻᎮHIႱ⋊⅃MͶOꟼỌЯꙄTUVWXYƸɒdɔbɘᎸǫʜiꞁʞ|mᴎoqpɿꙅƚuvwxʏƹ"
  #t)

(define-script-translation mathbb
  " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  " 𝔸𝔹ℂ𝔻𝔼𝔽𝔾ℍ𝕀𝕁𝕂𝕃𝕄ℕ𝕆ℙℚℝ𝕊𝕋𝕌𝕍𝕎𝕏𝕐ℤ𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡")

(define-script-translation smallcaps
  " ABCDEFGHIJKLMNOPRSTUVWYZ"
  " ᴀʙᴄᴅᴇꜰɢʜɪᴊᴋʟᴍɴᴏᴘʀsᴛᴜᴠᴡʏᴢ")

(define-script-translation fbchange
  " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789&_?!\"'.,;" ; nah: ԁг
  " аbсdеfghіјklmոοрԛrѕtυѵԝхуzАВСDЕFGHӀЈКLМNОРԚRЅТUѴԜXҮZ0123456789&_?!\"'.,;") ; асеіјոοрԛυѵѕԝхуAВСЕКӀЈМОРԚЅТѴԜХҮ
