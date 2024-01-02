///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "../BlockMap.hpp"


namespace Langulus::Anyness
{
   
   /// Semantically transfer the members of one map onto another              
   ///   @tparam TO - the type of map we're transferring to                   
   ///   @param other - the map and semantic to transfer from                 
   template<CT::Map TO, template<class> class S, CT::Map FROM>
   requires CT::Semantic<S<FROM>> LANGULUS(INLINED)
   void BlockMap::BlockTransfer(S<FROM>&& other) {
      mKeys.mCount = other->mKeys.mCount;

      if constexpr (not CT::TypedMap<TO>) {
         // TO is not statically typed                                  
         mKeys.mType = other->GetKeyType();
         mValues.mType = other->GetValueType();
         mKeys.mState = other->mKeys.mState;
         mValues.mState = other->mValues.mState;
      }
      else {
         // TO is statically typed                                      
         mKeys.mType = MetaDataOf<typename TO::Key>();
         mValues.mType = MetaDataOf<typename TO::Value>();
         mKeys.mState = other->mKeys.mState + DataState::Typed;
         mValues.mState = other->mStateValues + DataState::Typed;
      }

      if constexpr (S<FROM>::Shallow) {
         mKeys.mRaw = other->mKeys.mRaw;
         mKeys.mReserved = other->mKeys.mReserved;
         mValues.mRaw = other->mValues.mRaw;
         mInfo = other->mInfo;

         if constexpr (S<FROM>::Keep) {
            // Move/Copy other                                          
            mKeys.mEntry = other->mKeys.mEntry;
            mValues.mEntry = other->mValues.mEntry;

            if constexpr (S<FROM>::Move) {
               if constexpr (not FROM::Ownership) {
                  // Since we are not aware if that block is referenced 
                  // or not we reference it just in case, and we also   
                  // do not reset 'other' to avoid leaks When using raw 
                  // BlockMaps, it's your responsibility to take care   
                  // of ownership.                                      
                  Keep();
               }
               else {
                  other->mKeys.ResetMemory();
                  other->mKeys.ResetState();
                  other->mValues.ResetMemory();
                  other->mValues.ResetState();
               }
            }
            else Keep();
         }
         else if constexpr (S<FROM>::Move) {
            // Abandon other                                            
            mKeys.mEntry = other->mKeys.mEntry;
            mValues.mEntry = other->mValues.mEntry;
            other->mValues.mEntry = nullptr;
         }
      }
      else {
         // We're cloning, so we guarantee, that data is no longer      
         // static                                                      
         mKeys.mState -= DataState::Static;
         mValues.mState -= DataState::Static;

         if constexpr (CT::TypedMap<TO> or CT::TypedMap<FROM>) {
            using B = Conditional<CT::Typed<FROM>, FROM, TO>;
            auto asFrom = reinterpret_cast<const B*>(&*other);
            auto asTo = reinterpret_cast<B*>(this);
            asTo->AllocateFresh(asFrom->GetReserved());

            // Clone info array                                         
            CopyMemory(asTo->mInfo, asFrom->mInfo, GetReserved() + 1);

            // Clone keys and values                                    
            auto info = asTo->GetInfo();
            const auto infoEnd = asTo->GetInfoEnd();
            auto dstKey = asTo->GetKeyHandle(0);
            auto dstVal = asTo->GetValueHandle(0);
            auto srcKey = asFrom->GetKeyHandle(0);
            auto srcVal = asFrom->GetValueHandle(0);
            while (info != infoEnd) {
               if (*info) {
                  dstKey.New(Clone(srcKey));
                  dstVal.New(Clone(srcVal));
               }

               ++info;
               ++dstKey; ++dstVal;
               ++srcKey; ++srcVal;
            }
         }
         else {
            // Use type-erased cloning                                  
            AllocateFresh(other->GetReserved());

            // Clone info array                                         
            CopyMemory(mInfo, other->mInfo, GetReserved() + 1);

            auto info = GetInfo();
            const auto infoEnd = GetInfoEnd();
            auto dstKey = GetKeyInner(0);
            auto dstVal = GetValueInner(0);
            auto srcKey = other->GetKeyInner(0);
            auto srcVal = other->GetValueInner(0);
            while (info != infoEnd) {
               if (*info) {
                  dstKey.CallUnknownSemanticConstructors(
                     1, Clone(srcKey));
                  dstVal.CallUnknownSemanticConstructors(
                     1, Clone(srcVal));
               }

               ++info;
               dstKey.Next(); dstVal.Next();
               srcKey.Next(); srcVal.Next();
            }
         }
      }
   }

} // namespace Langulus::Anyness
