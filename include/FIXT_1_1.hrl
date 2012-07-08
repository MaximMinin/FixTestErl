-type length () :: int().
-type localmmktdate () :: string().
-type int () :: number().
-type tagNum () :: string().
-type seqNum () :: string().
-type numInGroup () :: string().
-type dayOfMonth () :: string().
-type qty () :: string().
-type price () :: string().
-type priceOffset () :: string().
-type amt () :: string().
-type percentage () :: string().
-type multipleCharValue () :: string().
-type multipleStringValue () :: string().
-type country () :: string().
-type currency () :: string().
-type exchange () :: string().
-type monthYear () :: string().
-type uTCTimestamp () :: string().
-type uTCTimeOnly () :: string().
-type uTCDateOnly () :: string().
-type localMktDate () :: string().
-type tZTimeOnly () :: string().
-type tZTimestamp () :: string().
-type data () :: string().
-type pattern () :: string().
-type tenor () :: string().
-type reserved100Plus () :: string().
-type reserved1000Plus () :: string().
-type reserved4000Plus () :: string().
-type xMLData () :: string().
-type beginSeqNo () :: seqNum.
-type beginString () :: string.
-type bodyLength () :: length.
-type checkSum () :: string.
-type endSeqNo () :: seqNum.
-type msgSeqNum () :: seqNum.
-type msgType () :: string.
-type newSeqNo () :: seqNum.
-type possDupFlag () :: originalTransmission|possibleDuplicate.
-type refSeqNum () :: seqNum.
-type senderCompID () :: string.
-type senderSubID () :: string.
-type sendingTime () :: uTCTimestamp.
-type targetCompID () :: string.
-type targetSubID () :: string.
-type text () :: string.
-type signature () :: data.
-type secureDataLen () :: length.
-type secureData () :: data.
-type signatureLength () :: length.
-type rawDataLength () :: length.
-type rawData () :: data.
-type possResend () :: originalTransmission|possibleResend.
-type encryptMethod () :: noneOther|pKCS|dES|pKCSDES|pGPDES|pGPDESMD5|pEMDESMD5.
-type heartBtInt () :: int.
-type testReqID () :: string.
-type onBehalfOfCompID () :: string.
-type onBehalfOfSubID () :: string.
-type origSendingTime () :: uTCTimestamp.
-type gapFillFlag () :: sequenceResetIgnoreMsgSeqNum|gapFillMessageMsgSeqNumFieldValid.
-type deliverToCompID () :: string.
-type deliverToSubID () :: string.
-type resetSeqNumFlag () :: no|yesResetSequenceNumbers.
-type senderLocationID () :: string.
-type targetLocationID () :: string.
-type onBehalfOfLocationID () :: string.
-type deliverToLocationID () :: string.
-type xmlDataLen () :: length.
-type xmlData () :: data.
-type messageEncoding () :: string.
-type encodedTextLen () :: length.
-type encodedText () :: data.
-type lastMsgSeqNumProcessed () :: seqNum.
-type refTagID () :: int.
-type refMsgType () :: string.
-type sessionRejectReason () :: invalidTagNumber|requiredTagMissing|tagNotDefinedForThisMessageType|undefinedTag|tagSpecifiedWithoutAValue|valueIsIncorrect|incorrectDataFormatForValue|decryptionProblem|signatureProblem|compidProblem|sendingtimeAccuracyProblem|invalidMsgtype|xMLValidationError|tagAppearsMoreThanOnce|tagSpecifiedOutOfRequiredOrder|repeatingGroupFieldsOutOfOrder|incorrectNumingroupCountForRepeatingGroup|nonDataValueIncludesFieldDelimiter|invalidUnsupportedApplicationVersion|other.
-type maxMessageSize () :: length.
-type testMessageIndicator () :: fales|true.
-type username () :: string.
-type password () :: string.
-type noHops () :: numInGroup.
-type hopCompID () :: string.
-type hopSendingTime () :: uTCTimestamp.
-type hopRefID () :: seqNum.
-type nextExpectedMsgSeqNum () :: seqNum.
-type newPassword () :: string.
-type applVerID () :: fIX27|fIX30|fIX40|fIX41|fIX42|fIX43|fIX44|fIX50|fIX50SP1.
-type cstmApplVerID () :: string.
-type refApplVerID () :: string.
-type refCstmApplVerID () :: string.
-type defaultApplVerID () :: string.
-type applExtID () :: int.
-type encryptedPasswordMethod () :: int.
-type encryptedPasswordLen () :: length.
-type encryptedPassword () :: data.
-type encryptedNewPasswordLen () :: length.
-type encryptedNewPassword () :: data.
-type refApplExtID () :: int.
-type defaultApplExtID () :: int.
-type defaultCstmApplVerID () :: string.
-type sessionStatus () :: sessionActive|sessionPasswordChanged|sessionPasswordDueToExpire|newSessionPasswordDoesNotComplyWithPolicy|sessionLogoutComplete|invalidUsernameOrPassword|accountLocked|logonsAreNotAllowedAtThisTime|passwordExpired.
-record( standardTrailer, {signatureLength :: signatureLength(), signature :: signature(), checkSum :: checkSum()}).
-record( repeatingReg_hopGrp_627, {hopCompID :: hopCompID(), hopSendingTime :: hopSendingTime(), hopRefID :: hopRefID()}).
-record( hopGrp, {repeatingReg_hopGrp_627 = [#repeatingReg_hopGrp_627{}]}).
-record( msgTypeGrp, {any}).
-record( standardHeader, {beginString :: beginString(), bodyLength :: bodyLength(), msgType :: msgType(), applVerID :: applVerID(), applExtID :: applExtID(), cstmApplVerID :: cstmApplVerID(), senderCompID :: senderCompID(), targetCompID :: targetCompID(), onBehalfOfCompID :: onBehalfOfCompID(), deliverToCompID :: deliverToCompID(), secureDataLen :: secureDataLen(), secureData :: secureData(), msgSeqNum :: msgSeqNum(), senderSubID :: senderSubID(), senderLocationID :: senderLocationID(), targetSubID :: targetSubID(), targetLocationID :: targetLocationID(), onBehalfOfSubID :: onBehalfOfSubID(), onBehalfOfLocationID :: onBehalfOfLocationID(), deliverToSubID :: deliverToSubID(), deliverToLocationID :: deliverToLocationID(), possDupFlag :: possDupFlag(), possResend :: possResend(), sendingTime :: sendingTime(), origSendingTime :: origSendingTime(), xmlDataLen :: xmlDataLen(), xmlData :: xmlData(), messageEncoding :: messageEncoding(), lastMsgSeqNumProcessed :: lastMsgSeqNumProcessed(), hopGrp :: #hopGrp{}}).
-record( heartbeat, {standardHeader :: #standardHeader{}, testReqID :: testReqID(), standardTrailer :: #standardTrailer{}}).
-record( testRequest, {standardHeader :: #standardHeader{}, testReqID :: testReqID(), standardTrailer :: #standardTrailer{}}).
-record( resendRequest, {standardHeader :: #standardHeader{}, beginSeqNo :: beginSeqNo(), endSeqNo :: endSeqNo(), standardTrailer :: #standardTrailer{}}).
-record( reject, {standardHeader :: #standardHeader{}, refSeqNum :: refSeqNum(), refTagID :: refTagID(), refMsgType :: refMsgType(), refApplVerID :: refApplVerID(), refApplExtID :: refApplExtID(), refCstmApplVerID :: refCstmApplVerID(), sessionRejectReason :: sessionRejectReason(), text :: text(), encodedTextLen :: encodedTextLen(), encodedText :: encodedText(), standardTrailer :: #standardTrailer{}}).
-record( sequenceReset, {standardHeader :: #standardHeader{}, gapFillFlag :: gapFillFlag(), newSeqNo :: newSeqNo(), standardTrailer :: #standardTrailer{}}).
-record( logout, {standardHeader :: #standardHeader{}, sessionStatus :: sessionStatus(), text :: text(), encodedTextLen :: encodedTextLen(), encodedText :: encodedText(), standardTrailer :: #standardTrailer{}}).
-record( logon, {standardHeader :: #standardHeader{}, encryptMethod :: encryptMethod(), heartBtInt :: heartBtInt(), rawDataLength :: rawDataLength(), rawData :: rawData(), resetSeqNumFlag :: resetSeqNumFlag(), nextExpectedMsgSeqNum :: nextExpectedMsgSeqNum(), maxMessageSize :: maxMessageSize(), msgTypeGrp :: #msgTypeGrp{}, testMessageIndicator :: testMessageIndicator(), username :: username(), password :: password(), newPassword :: newPassword(), encryptedPasswordMethod :: encryptedPasswordMethod(), encryptedPasswordLen :: encryptedPasswordLen(), encryptedPassword :: encryptedPassword(), encryptedNewPasswordLen :: encryptedNewPasswordLen(), encryptedNewPassword :: encryptedNewPassword(), sessionStatus :: sessionStatus(), defaultApplVerID :: defaultApplVerID(), defaultApplExtID :: defaultApplExtID(), defaultCstmApplVerID :: defaultCstmApplVerID(), text :: text(), encodedTextLen :: encodedTextLen(), encodedText :: encodedText(), standardTrailer :: #standardTrailer{}}).
-record( xMLnonFIX, {any}).