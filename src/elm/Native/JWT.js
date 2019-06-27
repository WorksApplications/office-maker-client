var _user$project$Native_JWT = function(localRuntime) {
  // Suppose the given token is a valid JWT
  function decodePayload(token) {
    const payload = token.split('.')[1];
    const payloadJson = JSON.parse(atob(payload));

    return {
      userId: payloadJson.userId,
      role: payloadJson.role.toLowerCase(),
    };
  }

  return {
    decodePayload: decodePayload,
  };
}();
