# Сервируя(Serving) API

Достаточно chit-chat о type-level комбинаторах уровня и представляя API как тип.
Можем ли мы уже иметь веб-сервис?

## Первый пример

Оснащенные(Equipped) некоторыми базовыми знаниями о том, как(the way) мы представляем API, давайте теперь
напишем наш первый веб-сервис(webservice).

Исходником(source) для этой секции руководства является грамотный(literate) haskell файл,
так сначала мы должны иметь некоторые расширения языка и импорт:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
```

**Важно**: модуль `Servant` происходит от пакета **servant-server**,
тот, который позволяет запускать веб-серверы, реализующие определенный тип API.  It
reexports all the types from the **servant** package that let you declare API
types as well as everything you need to turn your request handlers into a
fully-fledged webserver. This means that in your applications, you can just add
**servant-server** as a dependency, import `Servant` and not worry about anything
else.

Мы напишем сервер, который будет обслуживать следующий API.

``` haskell
type UserAPI1 = "users" :> Get '[JSON] [User]
```

Here's what we would like to see when making a GET request to `/users`.

``` javascript
[ {"name": "Isaac Newton", "age": 372, "email": "isaac@newton.co.uk", "registration_date": "1683-03-01"}
, {"name": "Albert Einstein", "age": 136, "email": "ae@mc2.org", "registration_date": "1905-12-01"}
]
```

Теперь давайте определим наш тип данных `User` и напишем несколько экземпляров для него.

``` haskell
data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User
```

Ничего забавного здесь не происходит. Но теперь мы можем определить наш список из двух пользователей.

``` haskell
users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]
```

Давайте также напишем наш тип API.

``` haskell ignore
type UserAPI1 = "users" :> Get '[JSON] [User]
```

Теперь мы можем позаботиться о написании актуального веб-сервиса, который будет обрабатывать запросы
к такому API. Это будет очень просто, сводятся к только(being reduced to just) одной
конечной точке. Тип веб-приложения определяется типом API,
через *семейство типа*(type family) с именем `Server`. (Семейства типов - это просто функции, которые
принимают типы в качестве входных и возвращаемых типов(take types as input and return types).) Семейство типа `Server` будет вычислять
правильный тип, что связка из запроса обработчиков должна иметь только из(that a bunch of request handlers should have just from the
соответствующего типа API.(corresponding API type.

Первое, что нужно знать о семействе типов(type family) `Server` это то, что за
сценой это будет управлять маршрутизацией, позволяя вам сосредоточиться только на бизнес
логике. Второе, что нужно знать, что для каждой конечной точки ваши обработчики будут
по умолчанию запускается в монаде `Handler`. Это очень легко преодолевается(is overridable),
как объясняется ближе к концу этого руководства. В-третьих, тип
значения, возвращаемый  в этой монаде, должен быть таким же, как и второй аргумент
комбинатора методов HTTP, используемого для соответствующей конечной точки. В нашем случае это
means we must provide a handler of type `Handler [User]`. Well,
we have a monad, let's just `return` our list:

``` haskell
server1 :: Server UserAPI1
server1 = return users1
```

Вот и все. Теперь мы можем превратить(turn) `server` в актуальный webserver используя
[wai](http://hackage.haskell.org/package/wai) и
[warp](http://hackage.haskell.org/package/warp):

``` haskell
userAPI :: Proxy UserAPI1
userAPI = Proxy

-- 'serve' происходит от servant и передаёт вам WAI приложение(Application),
-- которое вы можете представить как "abstract" web приложение,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI server1
```

`userAPI` bit is, alas, boilerplate (we need it to guide type inference).
Но это примерно столько же шаблонный, как вы получите(But that's about as much boilerplate as you get).

И все готово! Давайте запустим наш веб-сервис на порте 8081.

``` haskell
main :: IO ()
main = run 8081 app1
```

Вы можете поместить все это в файл или просто заграбить(grab) [servant's
repo](http://github.com/haskell-servant/servant) и посмотрите в директорию
*doc/tutorial*. This code (the source of this web page) is in
*doc/tutorial/Server.lhs*.

Если вы запустите его, вы можете перейти на `http://localhost:8081/users` в вашем браузере или
query it with curl and you see:

``` bash
$ curl http://localhost:8081/users
[{"email":"isaac@newton.co.uk","registration_date":"1683-03-01","age":372,"name":"Isaac Newton"},{"email":"ae@mc2.org","registration_date":"1905-12-01","age":136,"name":"Albert Einstein"}]
```

## Больше конечных точек(More endpoints)

Что, если мы хотим более чем одну конечную точку? Давайте добавим `/albert` и `/isaac` to
view the corresponding users encoded in JSON.

``` haskell
type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User
```

И давайте адаптируем наш код немного.

``` haskell
isaac :: User
isaac = User "Isaac Newton" 372 "isaac@newton.co.uk" (fromGregorian 1683 3 1)

albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org" (fromGregorian 1905 12 1)

users2 :: [User]
users2 = [isaac, albert]
```

Теперь, так же, как мы разделяем различные конечные точки в `UserAPI` с `:<|>`, мы
собираемся разделить обработчики тоже с `:<|>`! Они должны быть представлены в
том же порядке как и в типе API.

``` haskell
server2 :: Server UserAPI2
server2 = return users2
     :<|> return albert
     :<|> return isaac
```

Вот и все! You can run this example in the same way that we showed для
`server1` и check out the data available at `/users`, `/albert` и `/isaac`.

## От комбинаторов к обработчикам аргументов(From combinators to handler arguments

Хорошо, мы можем легко писать тривиальные веб-сервисы, но ни один из них не использует
комбинатор "фантазию(fancy)" из servant. Давайте рассмотрим это и сразу же используем `QueryParam`,
`Capture` и `ReqBody`. Вы увидите, как каждое вхождение(occurence) этих
комбинаторов в конечной точке делает соответствующий обработчик получает(the corresponding handler receive an)
аргумент соответствующего типа автоматически(argument of the appropriate type automatically). Вам не нужно беспокоиться о
вручную искать захваты(captures) URL или параметры строки запроса, или
декодировании/кодировании данных из/в JSON. Никогда.

Мы собираемся использовать следующие типы данных и функции для реализации
сервер для `API`.

``` haskell
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

data Position = Position
  { xCoord :: Int
  , yCoord :: Int
  } deriving Generic

instance ToJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic

instance ToJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance FromJSON ClientInfo
instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving Generic

instance ToJSON Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'

  where from'    = "great@company.com"
        to'      = clientEmail c
        subject' = "Hey " ++ clientName c ++ ", we miss you!"
        body'    = "Hi " ++ clientName c ++ ",\n\n"
                ++ "Since you've recently turned " ++ show (clientAge c)
                ++ ", have you checked out our latest "
                ++ intercalate ", " (clientInterestedIn c)
                ++ " products? Give us a visit!"
```

Мы можем реализовать обработчики для этих трех конечных точек:

``` haskell
server3 :: Server API
server3 = position
     :<|> hello
     :<|> marketing

  where position :: Int -> Int -> Handler Position
        position x y = return (Position x y)

        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n

        marketing :: ClientInfo -> Handler Email
        marketing clientinfo = return (emailForClient clientinfo)
```

Вы это видели? Типы для ваших обработчиков менялись именно так, как нам(to be just what we)
нужно! В частности:

  - `Capture "something" a` становится аргументом типа `a` (для `position`);
  - `QueryParam "something" a` становится аргументом типа `Maybe a` (потому что
an endpoint can technically be accessed without specifying any query
string parameter, we decided to "force" handlers to be aware that the
parameter might not always be there);

  - `ReqBody contentTypeList a` становится аргументом типа `a`;

Вот и все. Here's the example in action:

``` bash
$ curl http://localhost:8081/position/1/2
{"xCoord":1,"yCoord":2}
$ curl http://localhost:8081/hello
{"msg":"Hello, anonymous coward"}
$ curl http://localhost:8081/hello?name=Alp
{"msg":"Hello, Alp"}
$ curl -X POST -d '{"clientName":"Alp Mestanogullari", "clientEmail" : "alp@foo.com", "clientAge": 25, "clientInterestedIn": ["haskell", "mathematics"]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/marketing
{"subject":"Hey Alp Mestanogullari, we miss you!","body":"Hi Alp Mestanogullari,\n\nSince you've recently turned 25, have you checked out our latest haskell, mathematics products? Give us a visit!","to":"alp@foo.com","from":"great@company.com"}
```

Для справки, здесь список некоторых комбинаторов из **servant**:

 > - `Delete`, `Get`, `Patch`, `Post`, `Put`: these do not become arguments. They provide the return type of handlers, which usually is `Handler <something>`.
 > - `Capture "something" a` становится аргументом типа `a`.
 > - `QueryParam "something" a`, `Header "something" a` all become arguments of type `Maybe a`, because there might be no value at all specified by the client for these.
 > - `QueryFlag "something"` превращается в аргумент типа `Bool`.
 > - `QueryParams "something" a` превращается в аргумент типа `[a]`.
 > - `ReqBody contentTypes a` превращается в аргумент типа `a`.

## Классы `FromHttpApiData`/`ToHttpApiData`

Подождите... Как **servant** узнаёт как декодировать `Int`-ы из URL? Или как
декодировать значение `ClientInfo` из тела запроса? Это касается этого и
следующих двух разделов.

`Capture`-ы и `QueryParam`-ы представлены некоторым текстовым значением в URLs.
`Header`-ы аналогично(similarly) представлены парой имени заголовка и
соответствующим (текстовым) значением в "метаданных" запроса(request's "metadata"). Как типы
декодируются из заголовков(headers), захватов(captures) и параметров запроса(query params), выражается в классе
`FromHttpApiData` (из пакета
[**http-api-data**](http://hackage.haskell.org/package/http-api-data)):

``` haskell ignore
class FromHttpApiData a where
  {-# MINIMAL parseUrlPiece | parseQueryParam #-}
  -- | Parse URL path piece.
  parseUrlPiece :: Text -> Either Text a
  parseUrlPiece = parseQueryParam

  -- | Parse HTTP header value.
  parseHeader :: ByteString -> Either Text a
  parseHeader = parseUrlPiece . decodeUtf8

  -- | Parse query param value.
  parseQueryParam :: Text -> Either Text a
  parseQueryParam = parseUrlPiece
```

Как вы можете видеть, до тех пор, пока вы предоставляете либо `parseUrlPiece` (для `Capture`-ов)
либо `parseQueryParam` (для `QueryParam`-ов), другие методы будут определены в
условиях этого(terms of this).

**http-api-data** обеспечивает достойное количество экземпляров, помощников(helpers) для определения новых(for defining new ones)
и замечательную документацию.

Больше нечего сказать(There's not much else to say) об этих классах. You will need instances for
them when using `Capture`, `QueryParam`, `QueryParams`, и `Header` с вашими
типами. Вам будут необходимы экзмепляры `FromHttpApiData` for server-side request
handlers и экзмепляры `ToHttpApiData` only when using
**servant-client**, как описано в [section about deriving haskell
functions to query an API](Client.html).

## Использование content-types с вашими типами данных

Тот же принцип действует(was operating) при декодировании запроса bodies из JSON, и
responses *into* JSON. (JSON is just the running example - you can do this with
any content-type.)

В этой секции представлено несколько(a couple) типов классов, предоставляемых **servant**, которые делают
всю эту работу.

### Истина за(The truth behind) `JSON`


Чем именно является `JSON`(тип as used in `Get '[JSON] User`)? Подобно 3
другим content-types предоставленными(provided out) из коробки **servant**, это действительно примитивный(dumb)
тип данных.

``` haskell ignore
data JSON
data PlainText
data FormUrlEncoded
data OctetStream
```

Очевидно, это не всё чем является `JSON`, иначе это было бы совершенно(it would be quite)
бессмысленно(pointless). Подобно большинству типов данных в **servant**, `JSON` в основном существует как(is mostly there as)
специальный *символ*(symbol)  связанный с кодированием (that's associated with encoding (resp. decoding) to (resp.
from)) формата *JSON*. Способ выполнения этой ассоциации может быть
разложен на два этапа(The way this association is performed can be decomposed into two steps).

Первым шагом является снабжение свойства(The first step is to provide a proper)
`MediaType` (из
[**http-media**](https://hackage.haskell.org/package/http-media-0.6.2/docs/Network-HTTP-Media.html))
представление для `JSON`, или для твоих собственных content-types. If you look at the
haddocks from this link, you can see that we just have to specify
`application/json` using the appropriate functions. In our case, we can just
use `(//) :: ByteString -> ByteString -> MediaType`. Точный способ определить(The precise way to specify)
`MediaType` это написать экземпляр для класса `Accept`:

``` haskell ignore
-- для справки:
class Accept ctype where
    contentType   :: Proxy ctype -> MediaType

instance Accept JSON where
    contentType _ = "application" // "json"
```

Второй шаг сосредоточен вокруг(is centered around) классов `MimeRender` и `MimeUnrender`.
Эти классы просто позволяют вам определить способ кодирования и декодирования
значений в или из вашего представления content-type.

``` haskell ignore
class Accept ctype => MimeRender ctype a where
    mimeRender :: Proxy ctype -> a -> ByteString
    -- альтернативно читаемый как:
    mimeRender :: Proxy ctype -> (a -> ByteString)
```

Учитывая content-type и некоторый пользовательский тип, `MimeRender` предоставляет функцию, которая
кодирует значения типа `a` в ленивые `ByteString`-и.

В этом случае `JSON` с этим легко справиться(In the case of `JSON`, this is easily dealt with)! Для любого типа `a` с
экземпляром `ToJSON`, мы можем отображать значения этого типа в JSON, используя
`Data.Aeson.encode`.

``` haskell ignore
instance ToJSON a => MimeRender JSON a where
  mimeRender _ = encode
```

И теперь класс `MimeUnrender`, который позволяет нам извлекать значения из ленивых
`ByteString`-ов, alternatively failing with an error string.

``` haskell ignore
class Accept ctype => MimeUnrender ctype a where
    mimeUnrender :: Proxy ctype -> ByteString -> Either String a
```

У нас также не так много работы сделать это либо(We don't have much work to do there either), `Data.Aeson.eitherDecode` is
precisely what we need. Однако, it only allows arrays and objects as toplevel
значения JSON и это доказало в нашем пути более чем помогает нам,(values and this has proven to get in our way more than help us so we wrote
поэтому мы написали нашу небольшую функцию вокруг(our own little function around) **aeson** и **attoparsec** (которая позволяет использовать любой тип)that allows any type of
значения JSON на уровне(at the toplevel) "документа JSON (document)". Here's the definition in case
you are curious.

``` haskell
eitherDecodeLenient :: FromJSON a => ByteString -> Either String a
eitherDecodeLenient input = do
    v :: Value <- parseOnly (Data.Aeson.Parser.value <* endOfInput) (cs input)
    parseEither parseJSON v
```

Эта функция - именно то, что нам нужно для нашего экземпляра `MimeUnrender`.

``` haskell ignore
instance FromJSON a => MimeUnrender JSON a where
    mimeUnrender _ = eitherDecodeLenient
```

И это весь код, который позволяет вам использовать `JSON` с `ReqBody`, `Get`,
`Post` и friends. Мы можем проверить наше понимание, внедряя(implementing) поддержку
для `HTML` content-type, so that users of your webservice can access an
HTML representation of the data they want, ready to be included in any HTML
document, e.g. using [jQuery's `load` function](https://api.jquery.com/load/),
simply by adding `Accept: text/html` to their request headers.

### Тематические исследования(Case-studies): **servant-blaze** и **servant-lucid**

В наши дни большинство хакеллеров, которые пишут свои пользовательские интерфейсы HTML непосредственно из
Haskell, используют либо [**blaze-html**](http://hackage.haskell.org/package/blaze-html)
либо [**lucid**](http://hackage.haskell.org/package/lucid). The best option для
**servant** is obviously to support both (and hopefully other templating
solutions!). Мы сначала собираемся посмотреть на **lucid**:

``` haskell
data HTMLLucid
```

Ещё раз, тип данных присутствует только в качестве символа для функций кодирования/декодирования,
за исключением того, что на этот раз мы будем беспокоиться только о кодировании, поскольку
**lucid** не предоставляет способ извлечения данных из HTML.

``` haskell
instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")
```

Обратите внимание, что этот экземпляр использует оператор `(/:)` из **http-media** который позволяет
позволяет нам указать дополнительную информацию о content-type, подобно кодировке(charset) здесь.

Экземпляры визуализации(The rendering instances) вызывают аналогичные(call similar) функции, которые принимают
типы с соответствующим(appropriate) экземпляром в "абстрактном" представлении HTML и
затем записывают это в `ByteString`.

``` haskell
instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml

-- let's also provide an instance for lucid's
-- 'Html' wrapper.
instance MimeRender HTMLLucid (Html a) where
    mimeRender _ = renderBS
```

Для **blaze-html** все работает очень похоже:

``` haskell
-- For this tutorial to compile 'HTMLLucid' и 'HTMLBlaze' have to be
-- distinct. Usually you would stick to one html rendering library and then
-- you can go with one 'HTML' type.
data HTMLBlaze

instance Accept HTMLBlaze where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToMarkup a => MimeRender HTMLBlaze a where
    mimeRender _ = renderHtml . Text.Blaze.Html.toHtml

-- while we're at it, just like for lucid we can
-- provide an instance for rendering blaze's 'Html' type
instance MimeRender HTMLBlaze Text.Blaze.Html.Html where
    mimeRender _ = renderHtml
```

И [**servant-blaze**](http://hackage.haskell.org/package/servant-blaze) и
[**servant-lucid**](http://hackage.haskell.org/package/servant-lucid) позволяют вам использовать
`HTMLLucid` и `HTMLBlaze` в любом content-type list as long as you provide an instance of the
соответствующего(appropriate) класса (`ToMarkup` для **blaze-html**, `ToHtml` для **lucid**).

We can now write a webservice that uses **servant-lucid** to show the `HTMLLucid`
content-type in action. We will be serving the following API:

``` haskell
type PersonAPI = "persons" :> Get '[JSON, HTMLLucid] [Person]
```

where `Person` is defined as follows:

``` haskell
data Person = Person
  { firstName :: String
  , lastName  :: String
  } deriving Generic -- для экземпляра JSON

instance ToJSON Person
```

Теперь, давайте научим **lucid** how to render a `Person` as a row in a table, и затем
список `Person` as a table with a row per person.

``` haskell
-- HTML serialization of a single person
instance ToHtml Person where
  toHtml person =
    tr_ $ do
      td_ (toHtml $ firstName person)
      td_ (toHtml $ lastName person)

  -- do not worry too much about this
  toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [Person] where
  toHtml persons = table_ $ do
    tr_ $ do
      th_ "first name"
      th_ "last name"

    -- this just calls toHtml on each person of the list
    -- and concatenates the resulting pieces of HTML together
    foldMap toHtml persons

  toHtmlRaw = toHtml
```

Мы создаём некоторые значения `Person` и serve them as a list:

``` haskell
people :: [Person]
people =
  [ Person "Isaac"  "Newton"
  , Person "Albert" "Einstein"
  ]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server4 :: Server PersonAPI
server4 = return people

app2 :: Application
app2 = serve personAPI server4
```

И мы готовы пойти:

``` bash
$ curl http://localhost:8081/persons
[{"lastName":"Newton","firstName":"Isaac"},{"lastName":"Einstein","firstName":"Albert"}]
$ curl -H 'Accept: text/html' http://localhost:8081/persons
<table><tr><td>first name</td><td>last name</td></tr><tr><td>Isaac</td><td>Newton</td></tr><tr><td>Albert</td><td>Einstein</td></tr></table>
# или просто укажите своему браузеру на(or just point your browser to) http://localhost:8081/persons
```

## Монада `Handler`(Обработчик)

В сердце обработчиков лежит монада в которой они выполняются(is the monad they run in), а именно newtype `Handler` around `ExceptT ServantErr IO`
([haddock documentation для `ExceptT`](http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html#t:ExceptT)).
Одно может удивить: почему это монада? Ответ заключается в том, что это
простейшая монада со следующими свойствами:

- it lets us both return a successful result (using `return`)
or "fail" with a descriptive error (using `throwError`);
- это позволяет нам выполнять(perform) IO, которая является абсолютно необходимым(vital), поскольку большинство веб-сервисов существуют
как интерфейсы к базам данных, которые мы взаимодействуем с в(interact with in) `IO`.

Let's recall some definitions.

``` haskell ignore
-- из пакета 'mtl' at
newtype ExceptT e m a = ExceptT (m (Either e a))
```

Короче говоря, это означает, что обработчик типа `Handler a` просто
эквивалентен вычислению типа `IO (Either ServantErr a)`, то есть, IO
действие, которое либо возвращает ошибку либо результат.

Модуль [`Control.Monad.Except`](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html#t:ExceptT)
из которого `ExceptT` стоит посмотреть(comes is worth looking at).
Возможно, самое главное(Perhaps most importantly), `ExceptT` и `Handler` являются экземпляры `MonadError`, так
`throwError` can be used to return an error from your handler (whereas `return`
        is enough to return a success).

Большинство из того, что вы будете делать в ваших обработчиках выполняется в некоторой(is running some) IO и,
в зависимости от результата, вы иногда можете захотеть бросить(to throw) какой-нибудь вид ошибки
и прервать(abort) ее раньше. В следующих двух разделах рассказывается, как это сделать.

### Выполнение(Performing) IO

Другими важными экземплярами из списка выше являются `MonadIO m => MonadIO
(ExceptT e m)`, и, следовательно, также `MonadIO Handler` as there is `MonadIO IO` instance..
[`MonadIO`](http://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-IO-Class.html)
это класс из пакета **transformers** определённый как:

``` haskell ignore
class Monad m => MonadIO m where
  liftIO :: IO a -> m a
```

So if you want to run any kind of
IO computation in your handlers, just use `liftIO`:

``` haskell
type IOAPI1 = "myfile.txt" :> Get '[JSON] FileContent

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

instance ToJSON FileContent

server5 :: Server IOAPI1
server5 = do
  filecontent <- liftIO (readFile "myfile.txt")
  return (FileContent filecontent)
```

### Failing, through `ServantErr`

If you want to explicitly fail at providing the result promised by an endpoint
using the appropriate HTTP status code (not found, unauthorized, etc) and some
error message, all you have to do is use the `throwError` function mentioned above
and provide it with the appropriate value of type `ServantErr`, which is
defined as:

``` haskell ignore
data ServantErr = ServantErr
    { errHTTPCode     :: Int
    , errReasonPhrase :: String
    , errBody         :: ByteString -- lazy bytestring
    , errHeaders      :: [Header]
    }
```

Many standard values are provided out of the box by the `Servant.Server`
module. If you want to use these values but add a body или some headers, just
use record update syntax:

``` haskell
failingHandler :: Handler ()
failingHandler = throwError myerr

  where myerr :: ServantErr
        myerr = err503 { errBody = "Прости дорогой..." }
```

Here's an example where we return a customised 404-Not-Found error message in
the response body if "myfile.txt" isn't there:

``` haskell
server6 :: Server IOAPI1
server6 = do
  exists <- liftIO (doesFileExist "myfile.txt")
  if exists
    then liftIO (readFile "myfile.txt") >>= return . FileContent
    else throwError custom404Err

  where custom404Err = err404 { errBody = "myfile.txt just isn't there, please leave this server alone." }
```

Here's how that server looks in action:

``` bash
$ curl --verbose http://localhost:8081/myfile.txt
[snip]
* Connected to localhost (127.0.0.1) port 8081 (#0)
> GET /myfile.txt HTTP/1.1
> User-Agent: curl/7.30.0
> Host: localhost:8081
> Accept: */*
>
< HTTP/1.1 404 Not Found
[snip]
myfile.txt just isnt there, please leave this server alone.

$ echo Hello > myfile.txt

$ curl --verbose http://localhost:8081/myfile.txt
[snip]
* Connected to localhost (127.0.0.1) port 8081 (#0)
> GET /myfile.txt HTTP/1.1
> User-Agent: curl/7.30.0
> Host: localhost:8081
> Accept: */*
>
< HTTP/1.1 200 OK
[snip]
< Content-Type: application/json
[snip]
{"content":"Hello\n"}
```

## Заголовки ответов(Response headers)

Чтобы добавить заголовки к вашему ответу, используйте
[addHeader](http://hackage.haskell.org/package/servant/docs/Servant-API-ResponseHeaders.html).
Обратите внимание, что это изменяет тип вашего API, как мы видим в следующем примере:

``` haskell
type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myHandler :: Server MyHandler
myHandler = return $ addHeader 1797 albert
```

Обратите внимание, что тип `addHeader header x` is different than the type of `x`!
И если вы добавите больше заголовков, больше заголовков появится в списке заголовков(header list):

``` haskell
type MyHeadfulHandler = Get '[JSON] (Headers '[Header "X-A-Bool" Bool, Header "X-An-Int" Int] User)

myHeadfulHandler :: Server MyHeadfulHandler
myHeadfulHandler = return $ addHeader True $ addHeader 1797 albert
```

But what if your handler only *sometimes* adds a header? If you declare that
your handler adds headers, and you don't add one, the return type of your
handler will be different than expected. To solve this, you have to explicitly
*not* add a header by using `noHeader`:

``` haskell
type MyMaybeHeaderHandler
  = Capture "withHeader" Bool :> Get '[JSON] (Headers '[Header "X-An-Int" Int] User)

myMaybeHeaderHandler :: Server MyMaybeHeaderHandler
myMaybeHeaderHandler x = return $ if x then addHeader 1797 albert
                                       else noHeader albert
```

## Обслуживание(Serving) статических файлов

**servant-server** также предоставляет возможность просто обслуживать содержимое каталога
по определенному пути в вашем веб-API. Как упоминалось ранее в этом документе,
комбинатор `Raw` can be used in your APIs to mean "подключите сюда любое приложение WAI(plug here any WAI
application)". Хорошо, **servant-server** provides a function to get a file and
directory serving WAI application, namely:

``` haskell ignore
-- exported by Servant и Servant.Server
serveDirectoryWebApp :: FilePath -> Server Raw
```

`serveDirectoryWebApp`'s argument must be a path to a valid directory.

Here's an example API that will serve some static files:

``` haskell
type StaticAPI = "static" :> Raw
```

And the server:

``` haskell
staticAPI :: Proxy StaticAPI
staticAPI = Proxy
```

``` haskell
server7 :: Server StaticAPI
server7 = serveDirectoryWebApp "static-files"

app3 :: Application
app3 = serve staticAPI server7
```

Этот сервер будет соответствовать любому запросу, путь которого начинается с `/static` и будет выглядеть
for a file at the path described by the rest of the request path, inside the
 *static-files/* directory of the path you run the program from.

Другими словами: Если client requests `/static/foo.txt`, the server will look for a file at
`./static-files/foo.txt`. Если этот файл существует, он будет успешным и будет служить(serve) файлу.
Если он не существует, обработчик завершит с кодом статуса `404`.

`serveDirectoryWebApp` использует некоторые стандартные настройки, которые соответствуют варианту использования
serving статических файлов для большинства веб-приложений. Вы можете узнать о других
параметрах в документации модуля `Servant.Utils.StaticFiles`.

## Вложенные API-ы

Давайте посмотрим, как вы можете определить API-ы модульным способом, избегая повторения.
Рассмотрим этот простой пример:

``` haskell
type UserAPI3 = -- view the user with given userid, in JSON
                Capture "userid" Int :> Get '[JSON] User

           :<|> -- delete the user with given userid. empty response
                Capture "userid" Int :> DeleteNoContent '[JSON] NoContent
```

Мы можем вместо вынести(factor out) `userid`:

``` haskell
type UserAPI4 = Capture "userid" Int :>
  (    Get '[JSON] User
  :<|> DeleteNoContent '[JSON] NoContent
  )
```

Однако, вы должны знать(you have to be aware), что это влияет на тип (has an effect on the type)
соответствующего `Server`:

``` haskell ignore
Server UserAPI3 = (Int -> Handler User)
             :<|> (Int -> Handler NoContent)

Server UserAPI4 = Int -> (    Handler User
                         :<|> Handler NoContent
                         )
```

В первом случае, each handler receives the *userid* argument. In the latter,
the whole `Server` takes the *userid* and has handlers that are just
computations in `Handler`, with no arguments. In other words:

``` haskell
server8 :: Server UserAPI3
server8 = getUser :<|> deleteUser

  where getUser :: Int -> Handler User
        getUser _userid = error "..."

        deleteUser :: Int -> Handler NoContent
        deleteUser _userid = error "..."

-- notice how getUser и deleteUser
-- have a different type! no argument anymore,
-- the argument directly goes to the whole Server
server9 :: Server UserAPI4
server9 userid = getUser userid :<|> deleteUser userid

  where getUser :: Int -> Handler User
        getUser = error "..."

        deleteUser :: Int -> Handler NoContent
        deleteUser = error "..."
```

Note that there's nothing special about `Capture` that lets you "factor it
out": this can be done with any combinator. Here are a few examples of APIs
with a combinator factored out for which we can write a perfectly valid
`Server`.

``` haskell
-- we just factor out the "users" path fragment
type API1 = "users" :>
  (    Get '[JSON] [User] -- user listing
  :<|> Capture "userid" Int :> Get '[JSON] User -- view a particular user
  )

-- we factor out the Request Body
type API2 = ReqBody '[JSON] User :>
  (    Get '[JSON] User -- just display the same user back, don't register it
  :<|> PostNoContent '[JSON] NoContent  -- register the user. empty response
  )

-- we factor out a Header
type API3 = Header "Authorization" Token :>
  (    Get '[JSON] SecretData -- get some secret data, if authorized
  :<|> ReqBody '[JSON] SecretData :> PostNoContent '[JSON] NoContent -- add some secret data, if authorized
  )

newtype Token = Token ByteString
newtype SecretData = SecretData ByteString
```

This approach lets you define APIs modularly and assemble them all into one big
API type only at the end.

``` haskell
type UsersAPI =
       Get '[JSON] [User] -- list users
  :<|> ReqBody '[JSON] User :> PostNoContent '[JSON] NoContent -- add a user
  :<|> Capture "userid" Int :>
         ( Get '[JSON] User -- view a user
      :<|> ReqBody '[JSON] User :> PutNoContent '[JSON] NoContent -- update a user
      :<|> DeleteNoContent '[JSON] NoContent -- delete a user
         )

usersServer :: Server UsersAPI
usersServer = getUsers :<|> newUser :<|> userOperations

  where getUsers :: Handler [User]
        getUsers = error "..."

        newUser :: User -> Handler NoContent
        newUser = error "..."

        userOperations userid =
          viewUser userid :<|> updateUser userid :<|> deleteUser userid

          where
            viewUser :: Int -> Handler User
            viewUser = error "..."

            updateUser :: Int -> User -> Handler NoContent
            updateUser = error "..."

            deleteUser :: Int -> Handler NoContent
            deleteUser = error "..."
```

``` haskell
type ProductsAPI =
       Get '[JSON] [Product] -- list products
  :<|> ReqBody '[JSON] Product :> PostNoContent '[JSON] NoContent -- add a product
  :<|> Capture "productid" Int :>
         ( Get '[JSON] Product -- view a product
      :<|> ReqBody '[JSON] Product :> PutNoContent '[JSON] NoContent -- update a product
      :<|> DeleteNoContent '[JSON] NoContent -- delete a product
         )

data Product = Product { productId :: Int }

productsServer :: Server ProductsAPI
productsServer = getProducts :<|> newProduct :<|> productOperations

  where getProducts :: Handler [Product]
        getProducts = error "..."

        newProduct :: Product -> Handler NoContent
        newProduct = error "..."

        productOperations productid =
          viewProduct productid :<|> updateProduct productid :<|> deleteProduct productid

          where
            viewProduct :: Int -> Handler Product
            viewProduct = error "..."

            updateProduct :: Int -> Product -> Handler NoContent
            updateProduct = error "..."

            deleteProduct :: Int -> Handler NoContent
            deleteProduct = error "..."
```

``` haskell
type CombinedAPI = "users" :> UsersAPI
              :<|> "products" :> ProductsAPI

server10 :: Server CombinedAPI
server10 = usersServer :<|> productsServer
```

Finally, we can realize the user and product APIs are quite similar and
abstract that away:

``` haskell
-- API для значений типа 'a'
-- индексируется значениями типа 'i'
type APIFor a i =
       Get '[JSON] [a] -- list 'a's
  :<|> ReqBody '[JSON] a :> PostNoContent '[JSON] NoContent -- add an 'a'
  :<|> Capture "id" i :>
         ( Get '[JSON] a -- view an 'a' given its "identifier" of type 'i'
      :<|> ReqBody '[JSON] a :> PutNoContent '[JSON] NoContent -- update an 'a'
      :<|> DeleteNoContent '[JSON] NoContent -- delete an 'a'
         )

-- Build the appropriate 'Server'
-- given the handlers of the right type.
serverFor :: Handler [a] -- handler for listing of 'a's
          -> (a -> Handler NoContent) -- handler for adding an 'a'
          -> (i -> Handler a) -- handler for viewing an 'a' given its identifier of type 'i'
          -> (i -> a -> Handler NoContent) -- updating an 'a' with given id
          -> (i -> Handler NoContent) -- deleting an 'a' given its id
          -> Server (APIFor a i)
serverFor = error "..."
-- implementation left as an exercise. contact us on IRC
-- or the mailing list if you get stuck!
```

When your API contains the `EmptyAPI` combinator, you'll want to use
`emptyServer` in the corresponding slot for your server, which will simply fail
with 404 whenever a request reaches it:

``` haskell
type CombinedAPI2 = API :<|> "empty" :> EmptyAPI

server11 :: Server CombinedAPI2
server11 = server3 :<|> emptyServer
```

## Использование другой монады для ваших обработчиков(handlers)

Помните, как `Server` превращает комбинаторы для методов HTTP в `Handler`? Хорошо, на самом деле(actually), есть еще кое-что(there's more to that). `Server` на самом деле
простой синоним типа(type synonym).

``` haskell ignore
type Server api = ServerT api Handler
```

`ServerT` - это фактическое семейство типов, которое вычисляет требуемые типы для
обработчиков, которые являются частью(that's part of the) класса `HasServer`. Это похоже на `Server` за исключением
того, что это принимает(it takes) другой параметр, который является монадой, в которой вы хотите, чтобы ваши обработчики выполняли(to run in)
или, в общем, возвращали типы ваших обработчиков. This third parameter is
used for specifying the return type of the handler for an endpoint, e.g when
computing `ServerT (Get '[JSON] Person) SomeMonad`. Результатом будет
`SomeMonad Person`.

Первый и главный вопрос, который может возникнуть тогда: как мы будем писать обработчики,
которые работают в другой монаде? How can we "bring back" the value from a given monad
into something **servant** can understand?

### Естественные преобразования(Natural transformations)

Если у нас есть функция, которая получает нам из `m a` в `n a`, для любого `a`, что
мы имеем?

``` haskell ignore
newtype m :~> n = NT { ($$) :: forall a. m a -> n a}
```

Например:

``` haskell
listToMaybeNT :: [] :~> Maybe
listToMaybeNT = NT listToMaybe  -- из Data.Maybe
```

(`NT` происходит от "natural transformation(eстественное преобразованиe)", в случае, если вам это интересно.)

Поэтому, если вы хотите написать обработчики, используя другую монаду / тип, чем `Handler`, скажем монаду `Reader String`, первое, что вам нужно
для подготовки это функция:

``` haskell ignore
readerToHandler :: Reader String :~> Handler
```

Давайте начнём с `readerToHandler'`. Мы, очевидно, должны выполнить вычисления `Reader`
путем предоставления ему(by supplying it with) `String`, подобно `"hi"`. Мы получаем от него `a`
и можем просто `return` его в `Handler`. Затем мы можем просто обернуть
эту функцию конструктором `NT` , чтобы она имела fancier тип.

``` haskell
readerToHandler' :: forall a. Reader String a -> Handler a
readerToHandler' r = return (runReader r "hi")

readerToHandler :: Reader String :~> Handler
readerToHandler = NT readerToHandler'
```

Мы можем написать простой веб-сервис с обработчиками, запущенными в `Reader String`.

``` haskell
type ReaderAPI = "a" :> Get '[JSON] Int
            :<|> "b" :> Get '[JSON] String

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

readerServerT :: ServerT ReaderAPI (Reader String)
readerServerT = a :<|> b

  where a :: Reader String Int
        a = return 1797

        b :: Reader String String
        b = ask
```

К сожалению, мы не можем использовать `readerServerT` как аргумент `serve`, потому что
`serve` хочет `Server ReaderAPI`, i.e., with handlers running в `Handler`. But there's a simple solution to this.

### Войти(Enter) `enter`

Это верно. Мы только что написали `readerToHandler`, это именно то, что нам
нужно применить ко всем обработчикам, чтобы заставить обработчиков иметь
правильный тип для `serve`. Будучи громоздким, чтобы сделать вручную(Being cumbersome to do by hand), мы предоставляем функцию
`enter` который принимает естественное преобразование между двумя параметризованными типами `m`
и `n` и `ServerT someapi m`, и возвращает `ServerT someapi n`.

В нашем случае мы можем завершить наш небольшой веб-сервис, используя `enter
readerToHandler` на наших обработчиках.

``` haskell
readerServer :: Server ReaderAPI
readerServer = enter readerToHandler readerServerT

app4 :: Application
app4 = serve readerAPI readerServer
```

This is the webservice in action:

``` bash
$ curl http://localhost:8081/a
1797
$ curl http://localhost:8081/b
"hi"
```

## Заключение

You're now equipped to write webservices/web-applications используя
**servant**. Остальная часть этого документа(document) фокусируется(focuses) на **servant-client**,
**servant-js** и **servant-docs**.
