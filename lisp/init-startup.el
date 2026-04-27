;;; init-startup --- Init for Startup  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'seq)
(require 'subr-x)

(declare-function nerd-icons-icon-for-dir "nerd-icons")
(declare-function nerd-icons-icon-for-file "nerd-icons")
(declare-function nerd-icons-octicon "nerd-icons")
(declare-function visual-fill-column-mode "visual-fill-column")
(declare-function project-known-project-roots "project")
(defvar recentf-list)


;; Index:
;; 01 Visual Assets
;; 02 Quote Library
;; 03 Render Helpers
;; 04 Startup Screen Entry
;; 05 Startup Configuration

;;; ============================================================================
;;; 01 Visual Assets
;;; ============================================================================

(defgroup emacs-startup nil
  "Custom startup screen."
  :group 'startup)

(defvar mine-emacs-logo
  (propertize
   "███████╗███╗░░░███╗░█████╗░░█████╗░░██████╗
██╔════╝████╗░████║██╔══██╗██╔══██╗██╔════╝
█████╗░░██╔████╔██║███████║██║░░╚═╝╚█████╗░
██╔══╝░░██║╚██╔╝██║██╔══██║██║░░██╗░╚═══██╗
███████╗██║░╚═╝░██║██║░░██║╚█████╔╝██████╔╝
╚══════╝╚═╝░░░░░╚═╝╚═╝░░╚═╝░╚════╝░╚═════╝░"
   'face `(:foreground ,(face-foreground 'font-lock-string-face)
                       :height 1.2)
   )
  "ASCII Art logo for EMACS.")

(defvar emacs-startup-icon-position 10
  "Position for Emacs Startup's icon.")

(defvar emacs-startup-space 12
  "Spaces for Emacs Startup.")

(defface emacs-cow-color
  '(
    (((background light)) :foreground "#407040")
    (((background dark)) :foreground "#659965")
    )
  "Face for cow and words."
  :group 'emacs-startup)

(defvar emacs-cow
  "
         o
          o   ^__^
           o  (oo)\\_______
              (__)\\       )\\/\\
                  ||----w |
                  ||     ||
  "
  "Cow from vim-startify.")

(defvar emacs-startup-char_top_bottom '(?- ?─)
  "Make box for words.")

(defvar emacs-startup-char_sides '(?| ?│)
  "Make box for words.")

(defvar emacs-startup-char_top_left '(?* ?╭)
  "Make box for words.")

(defvar emacs-startup-char_top_right '(?* ?╮)
  "Make box for words.")

(defvar emacs-startup-char_bottom_right '(?* ?╯)
  "Make box for words.")

(defvar emacs-startup-char_bottom_left '(?* ?╰)
  "Make box for words.")

(defvar emacs-startup-filename-length 50
  "Max length of filename.")

(defvar emacs-startup-box-width 56
  "The width of box.")



;;; ============================================================================
;;; 02 Quote Library
;;; ============================================================================

(defvar emacs-startup-predefined-quotes
  (list
   '("Debugging is twice as hard as writing the code in the first place. Therefore, if you write the code as cleverly as possible, you are, by definition, not smart enough to debug it." "- Brian Kernighan")
   '("If you don't finish then you're just busy, not productive.")
   '("Adapting old programs to fit new machines usually means adapting new machines to behave like old ones." "- Alan Perlis")
   '("Fools ignore complexity. Pragmatists suffer it. Some can avoid it. Geniuses remove it." "- Alan Perlis")
   '("It is easier to change the specification to fit the program than vice versa." "- Alan Perlis")
   '("Simplicity does not precede complexity, but follows it." "- Alan Perlis")
   '("Optimization hinders evolution." "- Alan Perlis")
   '("Recursion is the root of computation since it trades description for time." "- Alan Perlis")
   '("It is better to have 100 functions operate on one data structure than 10 functions on 10 data structures." "- Alan Perlis")
   '("There is nothing quite so useless as doing with great efficiency something that should not be done at all." "- Peter Drucker")
   '("If you don't fail at least 90% of the time, you're not aiming high enough." "- Alan Kay")
   '("I think a lot of new programmers like to use advanced data structures and advanced language features as a way of demonstrating their ability. I call it the lion-tamer syndrome. Such demonstrations are impressive, but unless they actually translate into real wins for the project, avoid them." "- Glyn Williams")
   '("I would rather die of passion than of boredom." "- Vincent Van Gogh")
   '("If a system is to serve the creative spirit, it must be entirely comprehensible to a single individual.")
   '("The computing scientist's main challenge is not to get confused by the complexities of his own making." "- Edsger W. Dijkstra")
   '("Progress in a fixed context is almost always a form of optimization. Creative acts generally don't stay in the context that they are in." "- Alan Kay")
   '("The essence of XML is this: the problem it solves is not hard, and it does not solve the problem well." "- Phil Wadler")
   '("A good programmer is someone who always looks both ways before crossing a one-way street." "- Doug Linder")
   '("Patterns mean \"I have run out of language.\"" "- Rich Hickey")
   '("Always code as if the person who ends up maintaining your code is a violent psychopath who knows where you live." "- John Woods")
   '("Unix was not designed to stop its users from doing stupid things, as that would also stop them from doing clever things.")
   '("Contrary to popular belief, Unix is user friendly. It just happens to be very selective about who it decides to make friends with.")
   '("Perfection is achieved, not when there is nothing more to add, but when there is nothing left to take away.")
   '("There are two ways of constructing a software design: One way is to make it so simple that there are obviously no deficiencies, and the other way is to make it so complicated that there are no obvious deficiencies." "- C.A.R. Hoare")
   '("If you don't make mistakes, you're not working on hard enough problems.""- Frank Wilczek")
   '("If you don't start with a spec, every piece of code you write is a patch.""- Leslie Lamport")
   '("Caches are bugs waiting to happen." "- Rob Pike")
   '("Abstraction is not about vagueness, it is about being precise at a new semantic level." "- Edsger W. Dijkstra")
   '("dd is horrible on purpose. It's a joke about OS/360 JCL. But today it's an internationally standardized joke. I guess that says it all.""- Rob Pike")
   '("All loops are infinite ones for faulty RAM modules.")
   '("All idioms must be learned. Good idioms only need to be learned once." "- Alan Cooper")
   '("For a successful technology, reality must take precedence over public relations, for Nature cannot be fooled." "- Richard Feynman")
   '("If programmers were electricians, parallel programmers would be bomb disposal experts. Both cut wires." "- Bartosz Milewski")
   '("Computers are harder to maintain at high altitude. Thinner air means less cushion between disk heads and platters. Also more radiation.")
   '("Almost every programming language is overrated by its practitioners." "- Larry Wall")
   '("Fancy algorithms are slow when n is small, and n is usually small." "- Rob Pike")
   '("Methods are just functions with a special first argument." "- Andrew Gerrand")
   '("Care about your craft." "Why spend your life developing software unless you care about doing it well?")
   '("Provide options, don't make lame excuses.", '', "Instead of excuses, provide options. Don't say it can't be done; explain what can be done.")
   '("Be a catalyst for change." "You can't force change on people. Instead, show them how the future might be and help them participate in creating it.")
   '("Make quality a requirements issue." "Involve your users in determining the project's real quality requirements.")
   '("Critically analyze what you read and hear." "Don't be swayed by vendors, media hype, or dogma. Analyze information in terms of you and your project.")
   '("DRY - Don't Repeat Yourself." "Every piece of knowledge must have a single, unambiguous, authoritative representation within a system.")
   '("Eliminate effects between unrelated things." "Design components that are self-contained, independent, and have a single, well-defined purpose.")
   '("Use tracer bullets to find the target." "Tracer bullets let you home in on your target by trying things and seeing how close they land.")
   '("Program close to the problem domain." "Design and code in your user's language.")
   '("Iterate the schedule with the code." "Use experience you gain as you implement to refine the project time scales.")
   '("Use the power of command shells." "Use the shell when graphical user interfaces don't cut it.")
   '("Always use source code control." "Source code control is a time machine for your work - you can go back.")
   '("Don't panic when debugging" "Take a deep breath and THINK! about what could be causing the bug.")
   '("Don't assume it - prove it." "Prove your assumptions in the actual environment - with real data and boundary conditions.")
   '("Write code that writes code." "Code generators increase your productivity and help avoid duplication.")
   '("Design With contracts." "Use contracts to document and verify that code does no more and no less than it claims to do.")
   '("Use assertions to prevent the impossible." "Assertions validate your assumptions. Use them to protect your code from an uncertain world.")
   '("Finish what you start." "Where possible, the routine or object that allocates a resource should be responsible for deallocating it.")
   '("Configure, don't integrate." "Implement technology choices for an application as configuration options, not through integration or engineering.")
   '("Analyze workflow to improve concurrency." "Exploit concurrency in your user's workflow.")
   '("Always design for concurrency." "Allow for concurrency, and you'll design cleaner interfaces with fewer assumptions.")
   '("Use blackboards to coordinate workflow." "Use blackboards to coordinate disparate facts and agents, while maintaining independence and isolation among participants.")
   '("Estimate the order of your algorithms." "Get a feel for how long things are likely to take before you write code.")
   '("Refactor early, refactor often." "Just as you might weed and rearrange a garden, rewrite, rework, and re-architect code when it needs it. Fix the root of the problem.")
   '("Test your software, or your users will." "Test ruthlessly. Don't make your users find bugs for you.")
   '("Don't gather requirements - dig for them." "Requirements rarely lie on the surface. They're buried deep beneath layers of assumptions, misconceptions, and politics.")
   '("Abstractions live longer than details." "Invest in the abstraction, not the implementation. Abstractions can survive the barrage of changes from different implementations and new technologies.")
   '("Don't think outside the box - find the box." "When faced with an impossible problem, identify the real constraints. Ask yourself: \"Does it have to be done this way? Does it have to be done at all?\"")
   '("Some things are better done than described." "Don't fall into the specification spiral - at some point you need to start coding.")
   '("Costly tools don't produce better designs." "Beware of vendor hype, industry dogma, and the aura of the price tag. Judge tools on their merits.")
   '("Don't use manual procedures." "A shell script or batch file will execute the same instructions, in the same order, time after time.")
   '("Coding ain't done 'til all the Tests run." "'Nuff said.")
   '("Test state coverage, not code coverage." "Identify and test significant program states. Just testing lines of code isn't enough.")
   '("English is just a programming language." "Write documents as you would write code: honor the DRY principle, use metadata, MVC, automatic generation, and so on.")
   '("Gently exceed your users' expectations." "Come to understand your users' expectations, then deliver just that little bit more.")
   '("Think about your work." "Turn off the autopilot and take control. Constantly critique and appraise your work.")
   '("Don't live with broken windows." "Fix bad designs, wrong decisions, and poor code when you see them.")
   '("Remember the big picture." "Don't get so engrossed in the details that you forget to check what's happening around you.")
   '("Invest regularly in your knowledge portfolio." "Make learning a habit.")
   '("It's both what you say and the way you say it." "There's no point in having great ideas if you don't communicate them effectively.")
   '("Make it easy to reuse." "If it's easy to reuse, people will. Create an environment that supports reuse.")
   '("There are no final decisions." "No decision is cast in stone. Instead, consider each as being written in the sand at the beach, and plan for change.")
   '("Prototype to learn." "Prototyping is a learning experience. Its value lies not in the code you produce, but in the lessons you learn.")
   '("Estimate to avoid surprises." "Estimate before you start. You'll spot potential problems up front.")
   '("Keep knowledge in plain text." "Plain text won't become obsolete. It helps leverage your work and simplifies debugging and testing.")
   '("Use a single editor well." "The editor should be an extension of your hand; make sure your editor is configurable, extensible, and programmable.")
   '("Fix the problem, not the blame." "It doesn't really matter whether the bug is your fault or someone else's - it is still your problem, and it still needs to be fixed.")
   '("\"select\" isn't broken." "It is rare to find a bug in the OS or the compiler, or even a third-party product or library. The bug is most likely in the application.")
   '("Learn a text manipulation language." "You spend a large part of each day working with text. Why not have the computer do some of it for you?")
   '("You can't write perfect software." "Software can't be perfect. Protect your code and users from the inevitable errors.")
   '("Crash early." "A dead program normally does a lot less damage than a crippled one.")
   '("Use exceptions for exceptional problems." "Exceptions can suffer from all the readability and maintainability problems of classic spaghetti code. Reserve exceptions for exceptional things.")
   '("Minimize coupling between modules." "Avoid coupling by writing \"shy\" code and applying the Law of Demeter.")
   '("Put abstractions in code, details in metadata." "Program for the general case, and put the specifics outside the compiled code base.")
   '("Design using services." "Design in terms of services-independent, concurrent objects behind well-defined, consistent interfaces.")
   '("Separate views from models." "Gain flexibility at low cost by designing your application in terms of models and views.")
   '("Don't program by coincidence." "Rely only on reliable things. Beware of accidental complexity, and don't confuse a happy coincidence with a purposeful plan.")
   '("Test your estimates." "Mathematical analysis of algorithms doesn't tell you everything. Try timing your code in its target environment.")
   '("Design to test." "Start thinking about testing before you write a line of code.")
   '("Don't use wizard code you don't understand." "Wizards can generate reams of code. Make sure you understand all of it before you incorporate it into your project.")
   '("Work with a user to think like a user." "It's the best way to gain insight into how the system will really be used.")
   '("Use a project glossary." "Create and maintain a single source of all the specific terms and vocabulary for a project.")
   '("Start when you're ready." "You've been building experience all your life. Don't ignore niggling doubts.")
   '("Don't be a slave to formal methods." "Don't blindly adopt any technique without putting it into the context of your development practices and capabilities.")
   '("Organize teams around functionality." "Don't separate designers from coders, testers from data modelers. Build teams the way you build code.")
   '("Test early. Test often. Test automatically." "Tests that run with every build are much more effective than test plans that sit on a shelf.")
   '("Use saboteurs to test your testing." "Introduce bugs on purpose in a separate copy of the source to verify that testing will catch them.")
   '("Find bugs once." "Once a human tester finds a bug, it should be the last time a human tester finds that bug. Automatic tests should check for it from then on.")
   '("Sign your work." "Craftsmen of an earlier age were proud to sign their work. You should be, too.")
   '("Think twice, code once.")
   '("No matter how far down the wrong road you have gone, turn back now.")
   '("Why do we never have time to do it right, but always have time to do it over?")
   '("Weeks of programming can save you hours of planning.")
   '("To iterate is human, to recurse divine." "- L. Peter Deutsch")
   '("Computers are useless. They can only give you answers." "- Pablo Picasso")
   '("The question of whether computers can think is like the question of whether submarines can swim." "- Edsger W. Dijkstra")
   '("It's ridiculous to live 100 years and only be able to remember 30 million bytes. You know, less than a compact disc. The human condition is really becoming more obsolete every minute." "- Marvin Minsky")
   '("The city's central computer told you? R2D2, you know better than to trust a strange computer!" "- C3PO")
   '("Most software today is very much like an Egyptian pyramid with millions of bricks piled on top of each other, with no structural integrity, but just done by brute force and thousands of slaves." "- Alan Kay")
   '("I've finally learned what \"upward compatible\" means. It means we get to keep all our old mistakes." "- Dennie van Tassel")
   '("There are two major products that come out of Berkeley: LSD and UNIX. We don't believe this to be a coincidence." "- Jeremy S. Anderson")
   '("The bulk of all patents are crap. Spending time reading them is stupid. It's up to the patent owner to do so, and to enforce them." "- Linus Torvalds")
   '("Controlling complexity is the essence of computer programming." "- Brian Kernighan")
   '("Complexity kills. It sucks the life out of developers, it makes products difficult to plan, build and test, it introduces security challenges, and it causes end-user and administrator frustration." "- Ray Ozzie")
   '("The function of good software is to make the complex appear to be simple." "- Grady Booch")
   '("There's an old story about the person who wished his computer were as easy to use as his telephone. That wish has come true, since I no longer know how to use my telephone." "- Bjarne Stroustrup")
   '("There are only two industries that refer to their customers as \"users\"." "- Edward Tufte")
   '("Most of you are familiar with the virtues of a programmer. There are three, of course: laziness, impatience, and hubris." "- Larry Wall")
   '("Computer science education cannot make anybody an expert programmer any more than studying brushes and pigment can make somebody an expert painter." "- Eric S. Raymond")
   '("Optimism is an occupational hazard of programming; feedback is the treatment." "- Kent Beck")
   '("First, solve the problem. Then, write the code." "- John Johnson")
   '("Measuring programming progress by lines of code is like measuring aircraft building progress by weight." "- Bill Gates")
   '("Don't worry if it doesn't work right. If everything did, you'd be out of a job." "- Mosher's Law of Software Engineering")
   '("A LISP programmer knows the value of everything, but the cost of nothing." "- Alan J. Perlis")
   '("All problems in computer science can be solved with another level of indirection." "- David Wheeler")
   '("Functions delay binding; data structures induce binding. Moral: Structure data late in the programming process." "- Alan J. Perlis")
   '("Easy things should be easy and hard things should be possible." "- Larry Wall")
   '("Nothing is more permanent than a temporary solution.")
   '("If you can't explain something to a six-year-old, you really don't understand it yourself." "- Albert Einstein")
   '("All programming is an exercise in caching." "- Terje Mathisen")
   '("Software is hard." "- Donald Knuth")
   '("They did not know it was impossible, so they did it!" "- Mark Twain")
   '("The object-oriented model makes it easy to build up programs by accretion. What this often means, in practice, is that it provides a structured way to write spaghetti code." "- Paul Graham")
   '("Question: How does a large software project get to be one year late?" "Answer: One day at a time!")
   '("The first 90% of the code accounts for the first 90% of the development time. The remaining 10% of the code accounts for the other 90% of the development time." "- Tom Cargill")
   '("In software, we rarely have meaningful requirements. Even if we do, the only measure of success that matters is whether our solution solves the customer's shifting idea of what their problem is." "- Jeff Atwood")
   '("If debugging is the process of removing bugs, then programming must be the process of putting them in." "- Edsger W. Dijkstra")
   '("640K ought to be enough for anybody." "- Bill Gates, 1981")
   '("To understand recursion, one must first understand recursion." "- Stephen Hawking")
   '("Developing tolerance for imperfection is the key factor in turning chronic starters into consistent finishers." "- Jon Acuff")
   '("Every great developer you know got there by solving problems they were unqualified to solve until they actually did it." "- Patrick McKenzie")
   '("The average user doesn't give a damn what happens, as long as (1) it works and (2) it's fast." "- Daniel J. Bernstein")
   '("Walking on water and developing software from a specification are easy if both are frozen." "- Edward V. Berard")
   '("Be curious. Read widely. Try new things. I think a lot of what people call intelligence boils down to curiosity." "- Aaron Swartz")
   '("What one programmer can do in one month, two programmers can do in two months." "- Frederick P. Brooks")
   )
  "Great Words.")



;;; ============================================================================
;;; 03 Render Helpers
;;; ============================================================================

(defvar initial-startup-screen-buffer-name "*Emacs*"
  "Buffer name for the custom startup screen.")

(defvar initial-startup-screen-recent-limit 10
  "Maximum number of recent files shown on the startup screen.")

(defvar initial-startup-screen-project-limit 10
  "Maximum number of project roots shown on the startup screen.")

(defvar initial-startup-screen-row-raise 0.2
  "Vertical raise used to center row text in taller startup lines.")

(defvar initial-startup-screen-section-raise 0.2
  "Vertical raise used to center section headers in taller startup lines.")

(defun initial-startup-screen--align-indent ()
  "Return the display-aligned indentation used by the banner."
  (propertize
   " "
   'display `(space :align-to ,(max 0 (- fill-column emacs-startup-space 70)))))

(defun initial-startup-screen--pad-right (string width)
  "Pad STRING on the right to WIDTH display columns."
  (concat string (make-string (max 0 (- width (string-width string))) ? )))

(defun initial-startup-screen--pad-left (string width)
  "Pad STRING on the left to WIDTH display columns."
  (concat (make-string (max 0 (- width (string-width string))) ? ) string))

(defun initial-startup-screen--truncate-right (string width)
  "Truncate STRING to WIDTH display columns."
  (if (<= (string-width string) width)
      string
    (truncate-string-to-width string (max 0 width) nil nil t)))

(defun initial-startup-screen--truncate-left (string width)
  "Keep the tail of STRING within WIDTH display columns."
  (cond
   ((<= width 0) "")
   ((<= (string-width string) width) string)
   ((<= width 3) (truncate-string-to-width string width))
   (t
    (let* ((prefix "...")
           (tail-width (- width (string-width prefix)))
           (start-column (max 0 (- (string-width string) tail-width))))
      (concat prefix
              (truncate-string-to-width
               string (string-width string) start-column))))))

(defun initial-startup-screen--vcenter (string raise &rest properties)
  "Return STRING with vertical RAISE plus PROPERTIES."
  (apply #'propertize string 'display `(raise ,raise) properties))

(defun initial-startup-screen--quote-parts (quote)
  "Return (BODY TITLE AUTHOR) from QUOTE."
  (let* ((strings (seq-filter #'stringp quote))
         (first (car strings))
         (second (cadr strings)))
    (cond
     ((null first) (list "" nil nil))
     ((null second) (list first nil nil))
     ((or (string-prefix-p "- " second)
          (string-suffix-p "said." second))
      (list first nil second))
     (t
      (list second first nil)))))

(defun initial-startup-screen--wrap-text (text width)
  "Wrap TEXT to WIDTH columns."
  (with-temp-buffer
    (let ((fill-column width))
      (insert text)
      (fill-region (point-min) (point-max))
      (split-string (string-trim-right (buffer-string)) "\n" t))))

(defun initial-startup-screen--insert-box-line (text width &optional align-right)
  "Insert one startup quote box line with TEXT and WIDTH.
When ALIGN-RIGHT is non-nil, right-align TEXT."
  (insert (initial-startup-screen--align-indent))
  (insert (cadr emacs-startup-char_sides)
          " "
          (if align-right
              (initial-startup-screen--pad-left text width)
            (initial-startup-screen--pad-right text width))
          " "
          (cadr emacs-startup-char_sides))
  (newline))

(defun initial-startup-screen--box-border-line (left right width)
  "Return a quote box border line with LEFT, RIGHT, and WIDTH."
  (concat (string left)
          (make-string (+ width 2) (cadr emacs-startup-char_top_bottom))
          (string right)))

(defun initial-startup-screen--box-content-line (text width &optional align-right)
  "Return a quote box content line containing TEXT within WIDTH.
When ALIGN-RIGHT is non-nil, right-align TEXT."
  (concat (string (cadr emacs-startup-char_sides))
          " "
          (if align-right
              (initial-startup-screen--pad-left text width)
            (initial-startup-screen--pad-right text width))
          " "
          (string (cadr emacs-startup-char_sides))))

(defun initial-startup-screen--cow-lines ()
  "Return the cow art lines without changing indentation."
  (let ((lines (split-string emacs-cow "\n")))
    (setq lines (seq-drop-while #'string-blank-p lines))
    (nreverse (seq-drop-while #'string-blank-p (nreverse lines)))))

(defun emacs-startup-create-a-cow ()
  "Create the quote banner."
  (let* ((begin-point (point))
         (quote (nth (random (length emacs-startup-predefined-quotes))
                     emacs-startup-predefined-quotes))
         (parts (initial-startup-screen--quote-parts quote))
         (body (nth 0 parts))
         (title (nth 1 parts))
         (author (nth 2 parts))
         (width emacs-startup-box-width)
         (box-lines
          (list
           (initial-startup-screen--box-border-line
            (cadr emacs-startup-char_top_left)
            (cadr emacs-startup-char_top_right)
            width))))
    (when title
      (setq box-lines
            (append
             box-lines
             (list
              (initial-startup-screen--box-content-line
               (initial-startup-screen--truncate-right title width)
               width)))))
    (dolist (line (initial-startup-screen--wrap-text body width))
      (setq box-lines
            (append box-lines
                    (list (initial-startup-screen--box-content-line
                           line width)))))
    (when author
      (setq box-lines
            (append
             box-lines
             (list
              (initial-startup-screen--box-content-line
               (initial-startup-screen--truncate-left author width)
               width t)))))
    (setq box-lines
          (append
           box-lines
           (list
            (initial-startup-screen--box-border-line
             (cadr emacs-startup-char_bottom_left)
             (cadr emacs-startup-char_bottom_right)
             width))))
    (let* ((cow-lines (initial-startup-screen--cow-lines))
           (box-width (apply #'max (mapcar #'string-width box-lines)))
           (cow-width (apply #'max (mapcar #'string-width cow-lines)))
           (cow-offset (max 0 (- box-width cow-width 2))))
      (dolist (line box-lines)
        (insert (initial-startup-screen--align-indent))
        (insert line)
        (newline))
      (newline)
      (dolist (line cow-lines)
        (insert (initial-startup-screen--align-indent))
        (insert (make-string cow-offset ? ) line)
        (newline)))
    (add-face-text-property begin-point (point) 'emacs-cow-color)))

(defun initial-startup-screen--octicon (name fallback)
  "Return nerd-icons octicon NAME, or FALLBACK when unavailable."
  (if (fboundp 'nerd-icons-octicon)
      (nerd-icons-octicon name)
    fallback))

(defun initial-startup-screen--file-icon (file)
  "Return an icon for FILE with a plain-text fallback."
  (cond
   ((and (file-directory-p file) (fboundp 'nerd-icons-icon-for-dir))
    (nerd-icons-icon-for-dir file))
   ((fboundp 'nerd-icons-icon-for-file)
    (nerd-icons-icon-for-file file))
   ((file-directory-p file) "[D]")
   (t "[F]")))

(defun initial-startup-screen--insert-section-header (title icon)
  "Insert section header with TITLE and ICON."
  (let* ((start (point))
         (prefix (concat
                  (make-string (max 0 (- emacs-startup-space 2)) ? )
                  (make-string emacs-startup-icon-position ?─)
                  " "
                  (initial-startup-screen--vcenter
                   icon initial-startup-screen-section-raise)
                  " "
                  (initial-startup-screen--vcenter
                   title initial-startup-screen-section-raise
                   'face 'bold)
                  (propertize " " 'display '(space-width 0.8))))
         (dash-count (max 0 (- fill-column (string-width prefix)))))
    (insert prefix (make-string dash-count ?─) "\n")
    (add-text-properties start (point) '(line-height 1.5 line-spacing 0.3))))

(defun initial-startup-screen--target-at (position)
  "Return startup target at POSITION."
  (or (get-text-property position 'initial-startup-target)
      (and (> position (point-min))
           (get-text-property (1- position) 'initial-startup-target))))

(defun initial-startup-screen-open-at-point ()
  "Open startup target at point."
  (interactive)
  (let ((target (initial-startup-screen--target-at (point))))
    (when target
      (find-file target))))

(defun initial-startup-screen-open-mouse (event)
  "Open startup target clicked by mouse EVENT."
  (interactive "e")
  (let* ((position (posn-point (event-start event)))
         (target (and position (initial-startup-screen--target-at position))))
    (when target
      (find-file target))))

(defvar initial-startup-screen-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'initial-startup-screen-open-at-point)
    (define-key map [mouse-1] #'initial-startup-screen-open-mouse)
    map)
  "Keymap for clickable startup screen rows.")

(defun initial-startup-screen--insert-link-line
    (target label icon &optional suffix tooltip truncate-left)
  "Insert a clickable line for TARGET with LABEL and ICON.
Optional SUFFIX is aligned at the right edge.  TOOLTIP overrides
the default help text.  When TRUNCATE-LEFT is non-nil, keep the
tail of LABEL."
  (let* ((start (point))
         (suffix (or suffix ""))
         (suffix-width (if (string-empty-p suffix) 0 (1+ (string-width suffix))))
         (label-width (max 10 (- fill-column
                                  emacs-startup-space
                                  (string-width icon)
                                  suffix-width
                                  4)))
         (display-label (if truncate-left
                            (initial-startup-screen--truncate-left label label-width)
                          (initial-startup-screen--truncate-right label label-width))))
    (insert (make-string emacs-startup-space ? )
            (initial-startup-screen--vcenter
             icon initial-startup-screen-row-raise)
            " "
            (initial-startup-screen--vcenter
             display-label initial-startup-screen-row-raise
             'face '(:inherit link :underline nil)))
    (unless (string-empty-p suffix)
      (insert (propertize
               " "
               'display `(space :align-to
                                ,(max (+ emacs-startup-space 20)
                                      (- fill-column
                                         (string-width suffix)
                                         emacs-startup-space)))))
      (insert (initial-startup-screen--vcenter
               suffix initial-startup-screen-row-raise
               'face '(:inherit font-lock-keyword-face))))
    (add-text-properties
     start (point)
     (list 'line-height 1.4
           'mouse-face 'highlight
           'cursor nil
           'pointer 'hand
           'help-echo (or tooltip target)
           'follow-link t
           'keymap initial-startup-screen-link-map
           'initial-startup-target target))
    (newline)))

(defun initial-startup-screen--format-relative-time (time)
  "Return a compact relative display for TIME."
  (let ((age (max 0 (floor (float-time (time-subtract (current-time) time))))))
    (if (< age 1209600)
        (let* ((unit (cond
                      ((< age 60) (list age "sec"))
                      ((< age 3600) (list (/ age 60) "min"))
                      ((< age 86400) (list (/ age 3600) "hour"))
                      (t (list (/ age 86400) "day"))))
               (value (max 1 (car unit)))
               (label (cadr unit)))
          (format "%d %s%s ago" value label (if (= value 1) "" "s")))
      (let ((system-time-locale "C"))
        (format-time-string
         (if (> (decoded-time-year (decode-time (current-time)))
                (decoded-time-year (decode-time time)))
             " %Y %b %d"
           "%b %d %H:%M")
         time)))))

(defun initial-startup-screen--recent-files ()
  "Return recent local files as (TIME . FILE) pairs."
  (let ((files (if (and (boundp 'recentf-list) recentf-list)
                   recentf-list
                 file-name-history))
        (items nil)
        (count 0))
    (catch 'done
      (dolist (file files)
        (when (and (stringp file)
                   (not (file-remote-p file)))
          (let ((attributes (ignore-errors (file-attributes file))))
            (when attributes
              (push (cons (file-attribute-modification-time attributes) file) items)
              (setq count (1+ count))
              (when (>= count initial-startup-screen-recent-limit)
                (throw 'done nil)))))))
    (nreverse items)))

(defun initial-startup-screen--project-roots ()
  "Return project roots shown on the startup screen."
  (when (fboundp 'project-known-project-roots)
    (let ((items nil)
          (count 0))
      (catch 'done
        (dolist (root (ignore-errors (project-known-project-roots)))
          (when (and (stringp root)
                     (not (file-remote-p root))
                     (file-directory-p root))
            (push root items)
            (setq count (1+ count))
            (when (>= count initial-startup-screen-project-limit)
              (throw 'done nil)))))
      (nreverse items))))


;;; ============================================================================
;;; 04 Startup Screen Entry
;;; ============================================================================

(defun initial-startup-screen-refresh (&optional buffer)
  "Render the startup screen into BUFFER and return it."
  (let ((buffer (or buffer (get-buffer-create initial-startup-screen-buffer-name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (special-mode)
        (setq buffer-read-only nil)
        (erase-buffer)
        (auto-save-mode -1)
        (when (fboundp 'visual-fill-column-mode)
          (visual-fill-column-mode 1))
        (newline 7)
        (emacs-startup-create-a-cow)
        (let ((start (point)))
          (insert (propertize
                   (concat (make-string 48 ? )
                           (format "Emacs started in %s" (emacs-init-time)))
                   'face `(:foreground ,(face-foreground 'font-lock-builtin-face)
                                       :height 0.8)
                   'display '(raise 0.2)))
          (add-text-properties start (point) '(line-height 1.35 v-adjust 0.3)))
        (newline 1)
        (initial-startup-screen--insert-section-header
         "Recent Files"
         (initial-startup-screen--octicon "nf-oct-history" "[H]"))
        (dolist (entry (initial-startup-screen--recent-files))
          (let* ((time (car entry))
                 (file (cdr entry))
                 (relative-time (initial-startup-screen--format-relative-time time)))
            (initial-startup-screen--insert-link-line
             file
             file
             (initial-startup-screen--file-icon file)
             relative-time
             (format-time-string "%Y-%m-%d %T" time)
             t)))
        (initial-startup-screen--insert-section-header
         "Projects"
         (initial-startup-screen--octicon "nf-oct-project_roadmap" "[P]"))
        (dolist (root (initial-startup-screen--project-roots))
          (initial-startup-screen--insert-link-line
           root
           root
           (initial-startup-screen--file-icon root)))
        (goto-char (point-min))
        (setq buffer-read-only t)))
    buffer))

(defun initial-startup-screen (&optional _scratch switch)
  "Return the custom startup buffer.
Optional _SCRATCH and SWITCH are accepted for compatibility with
older calls.  When SWITCH is non-nil, display the buffer."
  (let ((buffer (initial-startup-screen-refresh)))
    (when switch
      (switch-to-buffer buffer))
    buffer))



;;; ============================================================================
;;; 05 Startup Configuration
;;; ============================================================================

(setq inhibit-startup-screen t
      initial-buffer-choice #'initial-startup-screen)

(provide 'init-startup)
;;; init-startup.el ends here.
