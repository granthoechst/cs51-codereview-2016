Overview
========

Any student who has taken Advanced Placement: Computer Science A should
find the material in this guide fairly familiar; it’s one of the
hallmark features of Java! Many programming languages (especially OCaml)
focus on the logic behind manipulating information and the “actions”
that are performed. Object Oriented Programming (**OOP**, for short),
brings the focus instead to the objects and data that are manipulated.

Features
--------

OOP involves a lot of features not present in other programming
paradigms to make working with objects easier:

-   **Classes.** Because of the focus on objects, OOP makes use of an
    abstraction called a class. A class encapsulates the properties and
    functions an object may use. For example, if you wanted to abstract
    the idea of a student, you might write a class that contained
    properties like “house”, “concentration”, or “favorite food.” In
    addition, your class might contain methods like “eatFavoriteFood” or
    “doHomework.”
    
    This is in contrast to functional programming, where functions take
    objects as parameters! In OOP, these functions are part of the class
    definition, and are available for any instance of the class.
    
    Classes generally provide additional methods called **constructors**
    that take a few parameters and return a new instance of the class.
    For example, using the above example, one constructor for a student
    might take three parameters (“house”, “concentration”, and “favorite
    food”).
    
    You might find thinking of classes this way helpful: functors return
    new modules in much the same way that constructors return new
    class instances. Functions contained in a module are similar to
    methods contained in a class (although class methods usually do not
    require the object as a parameter, like module functions do). A
    class definition is similar to module signatures in that the class
    definition is not an object itself; it merely provides a template
    for concrete objects to conform to.

-   **Inheritance.** Objects can often be thought of in a hierarchical
    fashion; for example, a student can be either a graduate or
    an undergraduate. Undergraduates can be further divided into STEM
    concentrators, humanities concentrators, and more.
    
    With other programming paradigms, you might have trouble defining
    the concept of a STEM concentrator in code; you would probably have
    to paste all the code you used to define a generic student into your
    definition for a STEM concentrator. To solve this problem, OOP makes
    use of a concept called inheritance. The idea is that STEM
    concentrators should probably have all the same properties and
    methods that generic students do; in OOP, we say that STEM
    concentrators **inherit** from generic students.
    
    Specifically, we say that a STEM concentrator class is a
    **subclass** of a generic student class. This makes defining lots of
    objects in a hierarchy a lot easier!

-   **Overriding.** One problem that arises when defining classes in
    this fashion is that more specific instances of a class might behave
    in a different way compared to the generic class. For example, STEM
    concentrators might always watch TV while doing homework. Generic
    students are dutiful and do homework without doing anything else.
    
    To allow for the possibility of defining more specific behavior for
    subclasses, OOP allows subclasses to **override** methods (and
    occasionally properties) of their superclasses.

-   **Interfaces.** These are similar to module signatures; interfaces
    specify what methods concrete classes should have available for use
    (although they provide no information about implementation details).
    Classes can signal that they conform to a particular interface, and
    functions can make use of this fact to use the methods that they now
    know are available for use.
    
    Also similarly to module signatures, interfaces allow you to expose
    only as much information as you would like about a class. This
    allows you to define internal functions in a class that the “outside
    world” cannot use.

-   **Dynamic Dispatch.** In OOP, methods are looked up at runtime for
    their specific implementation; this means that even if a function is
    passed a shape object, the exact implementation of the area method
    isn’t known until it’s called. This is what enables programmers to
    pass any concrete shape class and have the correct methods be used
    at runtime. In addition, methods can call upon each other at
    compilation time, instead of having to be defined recursively
    (remember how you had to define `'a stream` and `a
    str` at the same time in Lab 6?). For this reason, OOP is
    **dynamic**; there’s a lot of flexibility in how your methods are
    defined!

There are many more features of OOP that merit discussion; take a look
at the Wikipedia
[**page**](https://www.wikiwand.com/en/Object-oriented_programming)
for more information!

Advantages
----------

The main reason why OOP is so powerful is it eliminates a lot of
extraneous coding that programmers have to do to handle many different
objects at once. OOP allows you to define methods that work with generic
objects instead of worrying about the details of any particular object.
In addition, you don’t have to re-type a lot of code that you might have
to otherwise, to define new objects. We’ll look at concrete examples of
these ideas in the next section.

Code Examples
=============

With Lab 7, you saw the introduction of a “shape” class interface that
provides several methods like “translate” and “scale”. You used this
definition to create the “rectangle”, “square”, and “circle” classes.
With normal OCaml, you might have defined a shape in the following
manner:

    type shape = 
      | Rect of point * float * float
      | Square of point * float
      | Circle of point * float
    ;;

    let area (s: shape) = 
      match s with 
      | Rect (_, w, h) -> w *. h
      | Square (_, w) -> w *. w
      | Circle (_, r) -> 3.14159 * r * r
    ;;

    let scale (s: shape) (f: float) : shape =
      match s with
      | Rect (p, w, h) -> Rect (p, w *. f, h *. f)
      | Square (p, w) -> Square (p, w *. f)
      | Circle (c, r) -> Circle (c, r *. f)
    ;;

This probably would have worked fine for the most part. You could have
defined functions that take care of common code that you might have
(like `bounding box` and `area` in Lab 7). Any
problems that you have would probably be pretty easy resolvable, since
you know every single possible shape... right up until you decided you
wanted to implement another shape called Triangle. Think about what
happens now. You have to go back through your code and add another case
for every single match statement you do on a shape! This is obviously
not ideal!

OOP makes adding a new kind of shape a lot easier:

    class type shape = 
    object
      method area: float
      method scale (f: float) : unit
    end

    type point = {x: float; y: float}

    class rect (lower_left: point) (w: float) (h: float) : shape =
    object
      val mutable pos = lower_left
      val mutable width = w
      val mutable height = h
      
      method area: float = w *. h
      method scale (f: float) =
        width <- width *. f;
        height <- height *. f
    end

    class circle (center: point) (r: float) : shape =
    object
      val mutable pos = center
      val mutable radius = r
      
      method area: float = 3.14159 * r * r
      method scale (f: float) =
        radius <- radius *. f
    end

Subclassing makes defining a square incredibly easy:

    class square (lower_left: point) (s: float) : shape = 
    object
      inherit rect lower_left s s
    end

While the initial definitions of the functions seem a little more
verbose (most of it is actually class properties like `pos`
and `width`), this ends up saving you a LOT more time when
implementing new classes like this:

    class triangle (p1: point) (p2: point) (p3: point) : shape = 
    object
      val mutable v1 = p1
      val mutable v2 = p2
      val mutable v3 = p3
      
      method area: float = 
        let val1 = v1.x *. (v2.y -. v3.y) in
        let val2 = v2.x *. (v3.y -. v1.y) in 
        let val3 = v3.x *. (v1.y -. v2.y) in
        abs_float (0.5 *. (val1 +. val2 +. val3))
      method scale: float = ()
        (* leaving this out, it's really long! *)
    end

Note that if one of your functions requires the area of a shape, you no
longer have to add an extra case for a triangle; you can just do
`s#area`! In addition, the exact code run when you call
`s#area` isn’t known at compilation time; you don’t know
whether the “Rectangle” area code or the “Circle” area code will be run!
The program has to wait until it’s passed a specific shape object to
know what code to run! This is the power of dynamic dispatch; you can
define your area function separately for each class, and the code will
know which method to call.

In this way, you can separate the object-focused logic away from the
meat of your projects. Defining a new shape doesn’t require you to
destroy the logic throughout your project; you simply have to define a
new class, and your project will handle the new shape just fine!

We can also see subclassing in action. Suppose we want to change our
square so that scaling scales from the center of the square instead of
the lower left corner (from lab)! We can easily define a new square
class that does so (assume we have a `translate` method, just
like in lab):

    class square_center_scale (p: point) (s: float) : shape =
    object
      inherit square p s as super

      method! scale (k: float) : unit =
        let (x1, y1) = super#center in
        (* scale *)
        let _ = super#scale k in
        let (x2, y2) = super#center in
        (* translate back to center *)
        super#translate ((x1 -. x2), (y1 -. y2))
    end ;;

With normal OCaml, you’d have had to modify your other functions too and
copy all of the code you had defining a square! Instead, you can focus
on writing the relevant function.

Similarly, you can define a new class type that inherits from your
original shape class type. For example, in lab, we had you define a new
quadrilateral type that adds one method called `sides`:

    class type quad =
    object
      inherit shape
      
      method sides: (float * float * float * float)
    end

This is just a peek at all of the features object-oriented programming
has to offer. Note that even in OCaml itself, this is not even close to
all of the things you can do with OOP! You don’t even need to use
classes in OCaml; the concept exists of an object in OCaml, without the
need to define a class. Try it out! Just type the following into your
REPL (`ocaml` or `utop`):

    let stuart = 
      object
        val mutable name = "Stuart M. Shieber"
        val mutable job = "Professor at Harvard University"
        
        method getName = name
        method getJob = job
        method teach =
          Printf.printf "Prepare to have your minds blown"
        method sleep =
          Printf.printf "Good night"
      end ;;
    stuart#teach;;
    stuart#sleep;;

For more specifics on OOP in OCaml, check out chapters 11 and 12 in your
Real World OCaml textbook or read it online
[**here**](https://realworldocaml.org/v1/en/html/objects.html).
