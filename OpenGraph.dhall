let OpenGraphSite = { locale : Text, name : Text }

let OpenGraphBasic = { title : Text, image : Text, url : Text }

let OpenGraphMediaInfo =
      { content : Text, secureUrl : Text, contentType : Text }

let OpenGraphVisualMedia =
      { info : OpenGraphMediaInfo, width : Integer, height : Integer }

let OpenGraphAudio = { info : OpenGraphMediaInfo }

let OpenGraphImage = { info : OpenGraphVisualMedia, alt : Text }

let OpenGraphVideo = { info : OpenGraphVisualMedia }

let OpenGraphProfile = { firstName : Text, lastName : Text, userName : Text }

let OpenGraphWebsite =
      { site : OpenGraphSite
      , basic : OpenGraphBasic
      , images :
          List
            { info :
                { info :
                    { content : Text, secureUrl : Text, contentType : Text }
                , width : Integer
                , height : Integer
                }
            , alt : Text
            }
      }

let OpenGraphArticle =
      { publishedTime : Text
      , modifiedTime : Text
      , expirationTime : Optional Text
      , authors :
          List
            < Item : { firstName : Text, lastName : Text, userName : Text }
            | Url : Text
            >
      , section : Text
      , tags : List Text
      }

let OpenGraphProfileItem = < Item : OpenGraphProfile | Url : Text >

let OpenGraph =
      < Web : OpenGraphWebsite
      | Article : OpenGraphArticle
      | Profile : OpenGraphProfile
      | Video : OpenGraphVideo
      | Audio : OpenGraphAudio
      | Image : OpenGraphImage
      >

in  { OpenGraphSite
    , OpenGraphBasic
    , OpenGraphMediaInfo
    , OpenGraphVisualMedia
    , OpenGraphAudio
    , OpenGraphImage
    , OpenGraphVideo
    , OpenGraphProfile
    , OpenGraphWebsite
    , OpenGraphArticle
    , OpenGraphProfileItem
    , OpenGraph
    }