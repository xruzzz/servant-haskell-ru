# Web API как тип

Исходником(source) для этой секции руководства является грамотный(literate) haskell файл,
так во-первых мы нуждаемся, чтобы иметь некоторые расширения языка и импорт:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ApiType where

import Data.Text
import Data.Time (UTCTime)
import Servant.API
```

Рассмотрим следующую неформальную спецификацию API:

 > Конечная точка на `/users` ожидает запрос GET с параметром строки запроса
 > `sortby` whose value can be one of `age` или `name` и возвращает
 > list/array of JSON objects describing users, with fields `age`, `name`,
 > `email`, `registration_date`".

Вы *должны* быть способны, чтобы это формализовать. И затем использовать формализованную версию, чтобы 
получить вам большую часть пути(to get you much of the way towards) в написаннии web-приложения. И все пути к
получению некоторых клиентских библиотек, и доку и более.

Как мы будем описывать это с помощью **servant**? Описанием конечной точки является старый добрый
Haskell **тип**:

``` haskell
type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registration_date :: UTCTime
}
```

Давайте разберём это ниже(Let's break that down):

- `"users"` говорит, что наша конечная точка будет доступна под `/users`;
- `QueryParam "sortby" SortBy`, где  `SortBy` определённый как `data SortBy = Age | Name`,
    говорит, что конечная точка has a query string parameter named `sortby`
    whose value will be extracted as a value of type `SortBy`.
- `Get '[JSON] [User]` говорит, что конечная точка будет доступна через HTTP
   запросы GET, returning a list of users encoded as JSON. You will see
   later how you can make use of this to make your data available under different
   formats, выбор производится в зависимости от [Accept
   header](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html) specified in
   the client's request.
- Оператор `:>` который разделяет различные "комбинаторы" , просто позволяет вам
  последовательности статических фрагментов пути(sequence static path fragments), URL захваты(captures) и другие комбинаторы.
  Упорядочение имеет значение только(The ordering only matters) для статических фрагментов путей и URL захватов(captures).
  `"users" :> "list-all" :> Get '[JSON] [User]`, эквивалентно `/users/list-all`, is
  obviously not the same as `"list-all" :> "users" :> Get '[JSON] [User]`, which
  is equivalent to `/list-all/users`. This means that sometimes `:>` is somehow
  equivalent to `/`, but sometimes it just lets you chain another combinator.

Совет: Если ваша конечная точка responds to `/` (the root path), just omit any combinators
that introduce path segments. E.g. the following api has only one endpoint on `/`:

``` haskell
type RootEndpoint =
  Get '[JSON] User
```

We can also describe APIs with multiple endpoints by using the `:<|>`
combinators. Here's an example:

``` haskell
type UserAPI2 = "users" :> "list-all" :> Get '[JSON] [User]
           :<|> "list-all" :> "users" :> Get '[JSON] [User]
```

**servant** обеспечивает достаточное количество комбинаторов из коробки, но вы всегда можете
написать свой собственный, когда вам это нужно. Вот краткий обзор наиболее
часто встречающихся комбинаторов, с которыми работает **servant**.

## Комбинаторы

### Статические строки

As you've already seen, you can use type-level strings (enabled with the
`DataKinds` language extension) for static path fragments. Chaining
them amounts to `/`-separating them in a URL.

``` haskell
type UserAPI3 = "users" :> "list-all" :> "now" :> Get '[JSON] [User]
              -- describes an endpoint reachable at:
              -- /users/list-all/now
```

### `Delete`, `Get`, `Patch`, `Post` и `Put`

Комбинатор `Get` определяется в терминах(terms) более общего `Verb`(Глагол):
``` haskell ignore
data Verb method (statusCode :: Nat) (contentType :: [*]) a
type Get = Verb 'GET 200
```

Существуют и другие предопределенные синонимы типов для других распространенных методов HTTP,
такие как, например:
``` haskell ignore
type Delete = Verb 'DELETE 200
type Patch  = Verb 'PATCH 200
type Post   = Verb 'POST 200
type Put    = Verb 'PUT 200
```

There are also variants that do not return a 200 status code, such
as for example:
``` haskell ignore
type PostCreated  = Verb 'POST 201
type PostAccepted = Verb 'POST 202
```

An endpoint always ends with a variant of the `Verb` combinator
(unless you write your own combinators). Examples:

``` haskell
type UserAPI4 = "users" :> Get '[JSON] [User]
           :<|> "admins" :> Get '[JSON] [User]
```

### `Capture` (Захват)

URL захваты являются сегментами пути URL, который является переменным и фактическое значение
которого фиксируется и передается обработчикам запросов. Во многих веб-платформах(web frameworks), you'll see
it written as in `/users/:userid`, with that leading `:` denoting that `userid`
is just some kind of variable name or placeholder. For instance, if `userid` is
supposed to range over all integers greater or equal to 1, our endpoint will
match requests made to `/users/1`, `/users/143` and so on.

Комбинатор `Capture` в **servant** takes a (type-level) string representing
the "name of the variable" and a type, which indicates the type we want to
decode the "captured value" to.

``` haskell ignore
data Capture (s :: Symbol) a
-- s :: Symbol just says that 's' must be a type-level string.
```

В некоторых веб-платформах(web frameworks), вы используете regex-ы для захватов. Мы используем
[`FromHttpApiData`](https://hackage.haskell.org/package/http-api-data/docs/Web-HttpApiData.html#t:FromHttpApiData)
class, which the captured value must be an instance of.

Examples:

``` haskell
type UserAPI5 = "user" :> Capture "userid" Integer :> Get '[JSON] User
                -- что эквивалентно 'GET /user/:userid'
                -- except that we explicitly say that "userid"
                -- must be an integer

           :<|> "user" :> Capture "userid" Integer :> DeleteNoContent '[JSON] NoContent
                -- что эквивалентно 'DELETE /user/:userid'
```

In the second case, `DeleteNoContent` specifies a 204 response code,
`JSON` specifies the content types on which the handler will match,
and `NoContent` says that the response will always be empty.

### `QueryParam`, `QueryParams`, `QueryFlag`

`QueryParam`, `QueryParams` and `QueryFlag` are about parameters in the query string,
i.e., those parameters that come after the question mark
(`?`) in URLs, like `sortby` in `/users?sortby=age`, whose value is
set to `age`. `QueryParams` lets you specify that the query parameter
is actually a list of values, which can be specified using
`?param=value1&param=value2`. This represents a list of values
composed of `value1` and `value2`. `QueryFlag` lets you specify a
boolean-like query parameter where a client isn't forced to specify a
value. Отсутствие или наличие имени параметра в строке запроса
определяет, считается ли этот параметр значением `True` или `False`.
Для примера, `/users?active` would list only
active users whereas `/users` would list them all.

Here are the corresponding data type declarations:

``` haskell ignore
data QueryParam (sym :: Symbol) a
data QueryParams (sym :: Symbol) a
data QueryFlag (sym :: Symbol)
```

Examples:

``` haskell
type UserAPI6 = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
                -- что эквивалентно 'GET /users?sortby={age, name}'

```

Again, your handlers don't have to deserialize these things (into, for example,
a `SortBy`). **servant** takes care of it.

### `ReqBody`

Каждый HTTP запрос может содержать некоторые дополнительные данные, которые сервер может использовать в своем
*теле*, и эти данные могут быть закодированы в любом формате -- пока сервер
это понимает. Это можно использовать, например, для конечной точки для создания новых
пользователей: вместо того, чтобы передавать каждое поле пользователя в виде отдельного параметра строки запроса
или что-то такое грязное, мы можем сгруппировать все данные в объект JSON.
Преимущество этого заключается в поддержке вложенных объектов.

**servant** комбинатор `ReqBody` принимает список типов контента, в которых
могут быть представлены данные, закодированные в теле запроса, и тип этих данных.
И, как вы могли догадаться, вам не нужно проверять заголовок типа контента
и самостоятельно выполнять десериализацию. Мы делаем это за вас. И вернёт `Bad
Request` или `Unsupported Content Type` as appropriate.

Вот объявление типа данных для него:

``` haskell ignore
data ReqBody (contentTypes :: [*]) a
```

Examples:

``` haskell
type UserAPI7 = "users" :> ReqBody '[JSON] User :> Post '[JSON] User
                -- - что эквивалентно 'POST /users' с объектом JSON
                --   описывающим User в теле запроса
                -- - возвращает User закодированного в JSON

           :<|> "users" :> Capture "userid" Integer
                        :> ReqBody '[JSON] User
                        :> Put '[JSON] User
                -- - что эквивалентно 'PUT /users/:userid' с объектом JSON
                --   описывающим User в теле запроса
                -- - возвращает User закодированного в JSON
```

### Request `Header`s

Заголовки запросов используются для различных целей: от кеширования до передачи данных,
связанных с использованием данных. Они состоят из имени заголовка и связанного с ним значения.
They consist of a header name and an associated value. Примером может служить `Accept: application/json`.

Комбинатор `Header` в **servant** takes a type-level string for the header
name and the type to which we want to decode the header's value (from some
textual representation), as illustrated below:

``` haskell ignore
data Header (sym :: Symbol) a
```

Вот пример, где мы заявляем, что конечная точка использует заголовок `User-Agent`
которая определяет название software/library used by
the client to send the request.

``` haskell
type UserAPI8 = "users" :> Header "User-Agent" Text :> Get '[JSON] [User]
```

### Типы содержимого(Content types)

До сих пор всякий раз, когда мы использовали комбинатор, содержащий список типов контента, 
мы всегда указывали `'[JSON]`. Однако, **servant** позволяет использовать несколько типов 
содержимого, а также позволяет определять собственные типы содержимого.

Четыре типы содержимого предоставлены из коробки пакета ядра **servant**:
`JSON`, `PlainText`, `FormUrlEncoded` и `OctetStream`. Если по какой-то непонятной
причине вы хотели, чтобы одна из ваших конечных точек предоставляла ваши пользовательские данные
в этих 4 форматах, вы должны написать тип API, как показано ниже:

``` haskell
type UserAPI9 = "users" :> Get '[JSON, PlainText, FormUrlEncoded, OctetStream] [User]
```

(Существуют и другие пакеты, которые предоставляют другие типы контента. Например
**servant-lucid** и **servant-blaze** allow to generate html pages (using
**lucid** и **blaze-html**) и оба имеют тип контента для html.)

Мы будем далее объяснять, как эти типы содержимого и ваши типы данных могут играть(play)
вместе в [section about serving an API](Server.html).

### Ответ(Response) `Headers`

Подобно HTTP-запросу, ответ, генерируемый веб-сервером, также может содержать
заголовки. **servant** предоставляет комбинатор `Headers` that carries a list of
`Header` types and can be used by simply wrapping the "return type" of an endpoint
with it.

``` haskell ignore
data Headers (ls :: [*]) a
```

Если вы хотите описать конечную точку, которая возвращает заголовок "User-Count" в каждом
ответе, you could write it as below:

``` haskell
type UserAPI10 = "users" :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User])
```

### Базовая аутентификация(Basic Authentication)

После того, как вы установили основные маршруты и семантику вашего API, пришло время
подумать о защите его частей. Аутентификация и авторизация - это широкие
и тонкие темы; поскольку servant начал исследовать это пространство, мы начали небольшим(small)
с одной из самых ранних схем аутентификации HTTP: [Базовой аутентификации(Basic Authentication)](https://en.wikipedia.org/wiki/Basic_access_authentication).

When protecting endpoints with basic authentication, we need to specify two items:

1. **realm** of authentication as per the Basic Authentication spec.
2. The datatype returned by the server after authentication is verified. This
    is usually a `User` или `Customer` type datatype.

Имея в виду эти два элемента, *servant* provides the following combinator:

``` haskell ignore
data BasicAuth (realm :: Symbol) (userData :: *)
```

Который используется так:

``` haskell
type ProtectedAPI11
     = UserAPI                              -- this is public
 :<|> BasicAuth "my-realm" User :> UserAPI2 -- this is protected by auth
```

### Пустые(Empty) APIs

Иногда полезно иметь возможность обобщать API по типу некоторой его части:

``` haskell
type UserAPI12 innerAPI
     = UserAPI             -- this is the fixed bit of the API
 :<|> "inner" :> innerAPI  -- this lets us put various other APIs under /inner
```

Если есть случай, когда у вас нет ничего лишнего для обслуживания, вы можете использовать
комбинатор `EmptyAPI`, чтобы указать это:

``` haskell
type UserAPI12Alone = UserAPI12 EmptyAPI
```
Это также хорошо работает как заполнитель для незавершенных частей API, пока он
находится в разработке, потому что, когда вы знаете, что там должно быть _что-то_ (something),
но вы еще не знаете, что. Подумайте об этом как о типе unit `()`.

### Межоперативность(Interoperability) с `wai`: `Raw`

Finally, we also include a combinator named `Raw`который обеспечивает выходную люк для базовой
низкоуровневой веб-библиотеки `wai`. It can be used when
you want to plug a [wai `Application`](http://hackage.haskell.org/package/wai)
into your webservice:

``` haskell
type UserAPI13 = "users" :> Get '[JSON] [User]
                 -- конечная точка /users

            :<|> Raw
                 -- requests to anything else than /users
                 -- go here, где  сервер будет пытаться
                 -- найти файл с правильным именем
                 -- в правильном пути
```

Одним из примеров этого является то, что вы хотите обслуживать каталог статических файлов вместе
с остальной частью вашего API. Но вы можете подключить все, что является
`Приложением`(Application), например целое веб-приложение, написанное в любой из
веб-платформ(web frameworks), поддерживающих `wai`.
