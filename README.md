# Scala html template generator

This is a scala project that allows you to write html templates, similar to Jinja2. Note that this is still work in progress and only allows for and if statements.

This is an example of a html template that is written by the user:
```html
<!DOCTYPE html>
<html>
    <head>

    </head>
    <body>
        <h1>Hello, World!</h1>
        {# for _ in myList #}
            <p>Repeated content</p>
        {# endfor #}
        <h2>Subtitle</h2>
        {# if myCondition #}
            <p>Conditional content</p>
        {# endif #}
        {# if secondCondition #}
            <p>More conditional content</p>
        {# endif #}
    </body>
</html>
```

The user can change the case class in the main function to inject the correct data into the template.
For example:
```scala
  case class MyJSON(myList: Array[String], myCondition: Boolean)
  val data = MyJSON(myList = Array("first", "second", "third"), myCondition = true, secondCondition = false)

  val x = generate(tokens, data.asJson.noSpaces)
```

The generated html file that can be send to a client will look like:
```html
<!DOCTYPE html>
<html>
    <head>

    </head>
    <body>
        <h1>Hello, World!</h1>
        
            <p>Repeated content</p>
        
            <p>Repeated content</p>
        
            <p>Repeated content</p>
        
        <h2>Subtitle</h2>
        
            <p>Conditional content</p>
        
    </body>
</html>
```

## Run this project yourself.
Run:
```bash
sbt compile; sbt run
```