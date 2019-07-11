var _user$project$Native_JWT = (function(localRuntime) {
  // Suppose the given token is a valid JWT
  function decodePayload(token) {
    const payload = token.split('.')[1];
    const payloadJson = JSON.parse(atob(payload));

    return {
      userId: payloadJson.userId,
      role: payloadJson.role.toLowerCase(),

      // Can I do the expiration check here and throw an exception in JS side?
      // I will do that in Elm side for now
      exp: payloadJson.exp
    };
  }

  return {
    decodePayload: decodePayload
  };
})();
