(ns portkey.aws
  (:require [clojure.string :as str])
  (:import [java.time.format DateTimeFormatter]
           [java.time ZonedDateTime]))

(def formatter (DateTimeFormatter/ofPattern "YYYY-MM-dd'T'HH:mm:ssX"))

(defn parse-arn [arn]
  (let [[_ _ _ region account & _] (str/split arn #":")]
    {:region region
     :account account}))

(defn swagger-doc [api-function-name function-arn {:keys [path path-args query-args content-type]}]
  {"swagger" "2.0"
   "info" {"version" (.format formatter (ZonedDateTime/now))
           "title" "portkey"}
   "basePath" "/portkey"
   "schemes" ["https"]
   "paths"
   {path
    {"get"
     {"consumes" ["application/json"]
      "produces" [content-type]
      "parameters"
      (concat
        (for [arg query-args]
          {"name" arg
           "in" "query"
           "required" "true"
           "type" "string"})
       (for [arg path-args]
         {"name" arg
          "in" "path"
          "required" "true"
          "type" "string"}))
      "responses"
      {"200"
       {"description" "200 response"
        "schema" {"$ref" "#/definitions/Empty"}}
       "500"
       {"description" "500 response"
        "schema" {"$ref" "#/definitions/Error"}}}
      "x-amazon-apigateway-integration"
      {"responses"
       {"default" {"statusCode" "200"}
        ".*error.*" {"statusCode" "500"}}
       "requestTemplates"
       {"application/json"
        "##  See http://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-mapping-template-reference.html\n##  This template will pass through all parameters including path, querystring, header, stage variables, and context through to the integration endpoint via the body/payload\n#set($allParams = $input.params())\n{\n\"body-json\" : $input.json('$'),\n\"params\" : {\n#foreach($type in $allParams.keySet())\n    #set($params = $allParams.get($type))\n\"$type\" : {\n    #foreach($paramName in $params.keySet())\n    \"$paramName\" : \"$util.escapeJavaScript($params.get($paramName))\"\n        #if($foreach.hasNext),#end\n    #end\n}\n    #if($foreach.hasNext),#end\n#end\n},\n\"stage-variables\" : {\n#foreach($key in $stageVariables.keySet())\n\"$key\" : \"$util.escapeJavaScript($stageVariables.get($key))\"\n    #if($foreach.hasNext),#end\n#end\n},\n\"context\" : {\n    \"account-id\" : \"$context.identity.accountId\",\n    \"api-id\" : \"$context.apiId\",\n    \"api-key\" : \"$context.identity.apiKey\",\n    \"authorizer-principal-id\" : \"$context.authorizer.principalId\",\n    \"caller\" : \"$context.identity.caller\",\n    \"cognito-authentication-provider\" : \"$context.identity.cognitoAuthenticationProvider\",\n    \"cognito-authentication-type\" : \"$context.identity.cognitoAuthenticationType\",\n    \"cognito-identity-id\" : \"$context.identity.cognitoIdentityId\",\n    \"cognito-identity-pool-id\" : \"$context.identity.cognitoIdentityPoolId\",\n    \"http-method\" : \"$context.httpMethod\",\n    \"stage\" : \"$context.stage\",\n    \"source-ip\" : \"$context.identity.sourceIp\",\n    \"user\" : \"$context.identity.user\",\n    \"user-agent\" : \"$context.identity.userAgent\",\n    \"user-arn\" : \"$context.identity.userArn\",\n    \"request-id\" : \"$context.requestId\",\n    \"resource-id\" : \"$context.resourceId\",\n    \"resource-path\" : \"$context.resourcePath\"\n    }\n}\n"},
       "uri"
       (str "arn:aws:apigateway:"
            (-> function-arn parse-arn :region)
            ":lambda:path/2015-03-31/functions/"
            function-arn
            "/invocations")
       "passthroughBehavior" "when_no_templates"
       "httpMethod" "POST"
       "contentHandling" "CONVERT_TO_TEXT"
       "type" "aws"}}}}
   "definitions"
   {"Empty" {"type" "object"
             "title" "Empty Schema"}
    "Error" {"type" "object"
             "properties" {"message" {"type" "string"}}
             "title" "Error Schema"}}})
