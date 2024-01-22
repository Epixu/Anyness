///                                                                           
/// Langulus::Anyness                                                         
/// Copyright (c) 2012 Dimo Markov <team@langulus.com>                        
/// Part of the Langulus framework, see https://langulus.com                  
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "../Block.hpp"


namespace Langulus::Anyness
{

   /// Check if type origin is the same as one of the provided types          
   ///   @attention ignores sparsity and cv-qualifiers                        
   ///   @tparam T1, TN... - the types to compare against                     
   ///   @return true if data type is similar to at least one of the types    
   template<CT::Block THIS, CT::Data T1, CT::Data...TN> LANGULUS(INLINED)
   constexpr bool Block::Is() const noexcept {
      if constexpr (CT::Typed<THIS>)
         return CT::SameAsOneOf<TypeOf<THIS>, T1, TN...>;
      else
         return mType and mType->template Is<T1, TN...>();
   }

   /// Check if type origin is the same as another                            
   ///   @attention ignores sparsity and cv-qualifiers                        
   ///   @param type - the type to check for                                  
   ///   @return true if this block contains similar data                     
   template<CT::Block THIS> LANGULUS(INLINED)
   bool Block::Is(DMeta type) const noexcept {
      return GetType<THIS>() &= type;
   }

   /// Check if unqualified type is the same as one of the provided types     
   ///   @attention ignores only cv-qualifiers                                
   ///   @tparam T1, TN... - the types to compare against                     
   ///   @return true if data type is similar to at least one of the types    
   template<CT::Block THIS, CT::Data T1, CT::Data...TN> LANGULUS(INLINED)
   constexpr bool Block::IsSimilar() const noexcept {
      if constexpr (CT::Typed<THIS>)
         return CT::SimilarAsOneOf<TypeOf<THIS>, T1, TN...>;
      else
         return mType and mType->template IsSimilar<T1, TN...>();
   }

   /// Check if unqualified type is the same as another                       
   ///   @attention ignores only cv-qualifiers                                
   ///   @param type - the type to check for                                  
   ///   @return true if this block contains similar data                     
   template<CT::Block THIS> LANGULUS(INLINED)
   bool Block::IsSimilar(DMeta type) const noexcept {
      return GetType<THIS>() |= type;
   }

   /// Check if this type is exactly one of the provided types                
   ///   @tparam T1, TN... - the types to compare against                     
   ///   @return true if data type matches at least one type                  
   template<CT::Block THIS, CT::Data T1, CT::Data...TN> LANGULUS(INLINED)
   constexpr bool Block::IsExact() const noexcept {
      if constexpr (CT::Typed<THIS>)
         return CT::ExactAsOneOf<TypeOf<THIS>, T1, TN...>;
      else
         return mType and mType->template IsExact<T1, TN...>();
   }

   /// Check if this type is exactly another                                  
   ///   @param type - the type to match                                      
   ///   @return true if data type matches type exactly                       
   template<CT::Block THIS> LANGULUS(INLINED)
   bool Block::IsExact(DMeta type) const noexcept {
      return GetType<THIS>() == type;
   }

   /// Check if contained data can be interpreted as a given type             
   ///   @attention direction matters, if block is dense                      
   ///   @tparam BINARY_COMPATIBLE - do we require for the type to be         
   ///      binary compatible with this container's type                      
   ///   @param type - the type check if current type interprets to           
   ///   @return true if able to interpret current type to 'type'             
   template<bool BINARY_COMPATIBLE, CT::Block THIS> LANGULUS(INLINED)
   bool Block::CastsToMeta(DMeta type) const {
      if constexpr (CT::Typed<THIS>) {
         //TODO can be further optimized
         return GetType()->template
            CastsTo<BINARY_COMPATIBLE or CT::Sparse<TypeOf<THIS>>>(type);
      }
      else {
         return mType and (BINARY_COMPATIBLE or mType->mIsSparse
            ? mType->CastsTo<true>(type)
            : mType->CastsTo(type));
      }
   }

   /// Check if contained data can be interpreted as a number of a type       
   /// For example: a Vec4 can interpret as float[4]                          
   ///   @attention direction matters, if block is dense                      
   ///   @tparam BINARY_COMPATIBLE - do we require for the type to be         
   ///      binary compatible with this container's type                      
   ///   @param type - the type check if current type interprets to           
   ///   @param count - the number of elements to interpret as                
   ///   @return true if able to interpret current type to 'type'             
   template<bool BINARY_COMPATIBLE, CT::Block THIS> LANGULUS(INLINED)
   bool Block::CastsToMeta(DMeta type, Count count) const {
      if constexpr (CT::Typed<THIS>)
         //TODO can be further optimized
         return not type or GetType<THIS>()->CastsTo(type, count);
      else
         return not mType or not type or mType->CastsTo(type, count);
   }

   /// Check if this container's data can be represented as type T            
   /// with nothing more than pointer arithmetic                              
   ///   @tparam T - the type to compare against                              
   ///   @tparam BINARY_COMPATIBLE - do we require for the type to be         
   ///      binary compatible with this container's type                      
   ///   @return true if contained data is reinterpretable as T               
   template<CT::Data T, bool BINARY_COMPATIBLE, CT::Block THIS> LANGULUS(INLINED)
   bool Block::CastsTo() const {
      //TODO can be further optimized
      return CastsToMeta<BINARY_COMPATIBLE, THIS>(MetaDataOf<T>());
   }

   /// Check if this container's data can be represented as a specific number 
   /// of elements of type T, with nothing more than pointer arithmetic       
   ///   @tparam T - the type to compare against                              
   ///   @tparam BINARY_COMPATIBLE - do we require for the type to be         
   ///      binary compatible with this container's type                      
   ///   @param count - the number of elements of T                           
   ///   @return true if contained data is reinterpretable as T               
   template<CT::Data T, bool BINARY_COMPATIBLE, CT::Block THIS> LANGULUS(INLINED)
   bool Block::CastsTo(const Count count) const {
      //TODO can be further optimized
      return CastsToMeta<BINARY_COMPATIBLE, THIS>(MetaDataOf<T>(), count);
   }
   
   /// Reinterpret contents of this Block as the type and state of another    
   /// You can interpret Vec4 as float[4] for example, or any other such      
   /// reinterpretation, as long as data remains tightly packed and aligned   
   /// No real conversion is performed, only pointer arithmetic               
   ///   @param pattern - the type of data to try interpreting as             
   ///   @return a block representing this block, interpreted as the pattern  
   template<CT::Block THIS, CT::Block B> LANGULUS(INLINED)
   B Block::ReinterpretAs(const B& pattern) const {
      if (IsEmpty() or IsSparse<THIS>()
      or IsUntyped<THIS>() or pattern.template IsUntyped<B>())
         return B {};

      if constexpr (CT::Typed<THIS, B>) {
         using T1 = TypeOf<THIS>;
         using T2 = TypeOf<B>;

         // Both containers are statically typed                        
         if constexpr (CT::BinaryCompatible<T1, T2>) {
            // 1:1 view for binary compatible types                     
            return B {Disown(Block{
               pattern.GetState() + DataState::Static,
               pattern.GetType(), mCount,
               mRaw, mEntry
            })};
         }
         else if constexpr (CT::POD<T1, T2>) {
            if constexpr (sizeof(T1) >= sizeof(T2)
                     and (sizeof(T1) %  sizeof(T2)) == 0) {
               // Larger view for binary compatible types               
               return B {Disown(Block{
                  pattern.GetState() + DataState::Static,
                  pattern.GetType(), mCount * (sizeof(T1) / sizeof(T2)),
                  mRaw, mEntry
               })};
            }
            else if constexpr (sizeof(T1) <= sizeof(T2)
                          and (sizeof(T2) %  sizeof(T1)) == 0) {
               // Smaller view for binary compatible types              
               return B {Disown(Block{
                  pattern.GetState() + DataState::Static,
                  pattern.GetType(), mCount / (sizeof(T2) / sizeof(T1)),
                  mRaw, mEntry
               })};
            }
            else LANGULUS_ERROR("Can't reinterpret POD types - not alignable");
         }
         else LANGULUS_ERROR("Can't reinterpret blocks - types are not binary compatible");
         //TODO add imposed base reinterprets here too, by statically scanning reflected bases?
      }
      else {
         // One of the blocks is type-erased, so do RTTI checks         
         // This also includes imposed base reinterpretations           
         // First, compare types and get a common base type if any      
         RTTI::Base common {};
         if (not CompareTypes(pattern, common) or not common.mBinaryCompatible)
            return B {};

         // Find how elements fit from one to another                   
         const Size baseBytes = (common.mType->mSize * common.mCount)
            / pattern.GetStride();
         const Size resultSize = pattern.IsEmpty()
            ? baseBytes : (baseBytes / pattern.mCount) * pattern.mCount;

         // Create a static view of the desired type                    
         return B {Disown(Block{
            pattern.mState + DataState::Static,
            pattern.mType, resultSize,
            mRaw, mEntry
         })};
      }
   }

   template<CT::Data T, CT::Block THIS> LANGULUS(INLINED)
   TAny<T> Block::ReinterpretAs() const {
      static_assert(CT::Dense<T>, "T must be dense");
      return ReinterpretAs<THIS>(Block::From<T>());
   }

   /// Get the memory block corresponding to a local member variable          
   ///   @attention assumes block is not empty                                
   ///   @param member - the member to get                                    
   ///   @param idx - the element to get member from                          
   ///   @return a static memory block                                        
   template<CT::Block THIS> LANGULUS(INLINED)
   Block Block::GetMember(const RTTI::Member& member, CT::Index auto idx) {
      LANGULUS_ASSUME(DevAssumes, not IsEmpty(),
         "Getting member from an empty block");
      const auto index = SimplifyIndex<THIS>(idx);
      return { 
         DataState::Member, member.GetType(), member.mCount,
         member.Get(mRaw + mType->mSize * index), mEntry
      };
   }

   template<CT::Block THIS> LANGULUS(INLINED)
   Block Block::GetMember(const RTTI::Member& member, CT::Index auto idx) const {
      auto result = const_cast<Block*>(this)->GetMember<THIS>(member, idx);
      result.MakeConst();
      return result;
   }

   /// Get the memory block corresponding to a base                           
   ///   @attention assumes block is not empty                                
   ///   @param meta - the type of the resulting base block                   
   ///   @param base - the base reflection to use                             
   ///   @return the static block for the base                                
   LANGULUS(INLINED)
   Block Block::GetBaseMemory(DMeta meta, const RTTI::Base& base) {
      return {
         DataState::Member, meta,
         base.mCount * (base.mBinaryCompatible ? GetCount() : 1),
         mRaw + base.mOffset, mEntry
      };
   }

   LANGULUS(INLINED)
   Block Block::GetBaseMemory(DMeta meta, const RTTI::Base& base) const {
      auto result = const_cast<Block*>(this)->GetBaseMemory(meta, base);
      result.MakeConst();
      return result;
   }

   /// Get the memory block corresponding to a base                           
   ///   @attention assumes block is not empty                                
   ///   @param base - the base reflection to use                             
   ///   @return the static block for the base                                
   LANGULUS(INLINED)
   Block Block::GetBaseMemory(const RTTI::Base& base) {
      return GetBaseMemory(base.mType, base);
   }

   LANGULUS(INLINED)
   Block Block::GetBaseMemory(const RTTI::Base& base) const {
      return GetBaseMemory(base.mType, base);
   }
   
   /// Mutate the block to a different type, if possible                      
   ///   @tparam T - the type to change to                                    
   ///   @tparam FORCE - insert even if types mismatch, by making this block  
   ///                   deep with provided type - use void to disable        
   ///   @return true if block was deepened to incorporate the new type       
   template<CT::Block THIS, CT::Data T, class FORCE> LANGULUS(INLINED)
   bool Block::Mutate() {
      if constexpr (CT::Typed<THIS>) {
         if constexpr (CT::Similar<TypeOf<THIS>, T>) {
            // No need to mutate - types are compatible                 
            return false;
         }
         else if constexpr (not CT::Void<FORCE> and IsDeep<THIS>()) {
            // Container is already deep, just make it deeper           
            Deepen<FORCE, true, THIS>();
            return true;
         }
         else LANGULUS_OOPS(Mutate, "Can't mutate to incompatible type");
      }
      else return Mutate<THIS, FORCE>(MetaDataOf<T>());
   }
   
   /// Mutate to another compatible type, deepening the container if allowed  
   ///   @tparam FORCE - insert even if types mismatch, by making this block  
   ///                   deep with provided type - use void to disable        
   ///   @param meta - the type to mutate into                                
   ///   @return true if block was deepened to incorporate the new type       
   template<CT::Block THIS, class FORCE>
   bool Block::Mutate(DMeta meta) {
      static_assert(not CT::Typed<THIS>,
         "Can't set type of a statically typed container");

      if (IsUntyped<THIS>() or (not mState.IsTyped() and mType->mIsAbstract
                                and IsEmpty() and meta->CastsTo(mType))
      ) {
         // Undefined/abstract containers can mutate freely             
         SetType<false, THIS>(meta);
      }
      else if (mType->IsSimilar(meta)) {
         // No need to mutate - types are compatible                    
         return false;
      }
      else if (not IsInsertable<THIS>(meta)) {
         // Not insertable because of reasons                           
         if constexpr (not CT::Void<FORCE>) {
            if (not IsTypeConstrained<THIS>()) {
               // Container is not type-constrained, so we can safely   
               // deepen it, to incorporate the new data                
               Deepen<FORCE, true, THIS>();
               return true;
            }

            LANGULUS_OOPS(Mutate, "Attempting to mutate incompatible "
               "type-constrained container");
         }
         else LANGULUS_OOPS(Mutate, "Can't mutate to incompatible type");
      }

      // Block may have mutated, but it didn't happen                   
      return false;
   }
   
   /// Set the data ID - use this only if you really know what you're doing   
   ///   @tparam CONSTRAIN - whether or not to enable type-constraint         
   ///   @param type - the type meta to set                                   
   template<bool CONSTRAIN, CT::Block THIS>
   void Block::SetType(DMeta type) {
      static_assert(not CT::Typed<THIS>,
         "Can't change type of a statically typed container");

      if (mType == type) {
         if constexpr (CONSTRAIN)
            MakeTypeConstrained();
         return;
      }
      else if (not mType) {
         mType = type;
         if constexpr (CONSTRAIN)
            MakeTypeConstrained();
         return;
      }

      LANGULUS_ASSERT(not IsTypeConstrained<THIS>(), Mutate,
         "Incompatible type");

      if (mType->CastsTo(type)) {
         // Type is compatible, but only sparse data can mutate freely  
         // Dense containers can't mutate because their destructors     
         // might be wrong later                                        
         LANGULUS_ASSERT(IsSparse<THIS>(), Mutate, "Incompatible type");
         mType = type;
      }
      else {
         // Type is not compatible, but container is not typed, so if   
         // it has no constructed elements, we can still mutate it      
         LANGULUS_ASSERT(IsEmpty(), Mutate, "Incompatible type");
         mType = type;
      }

      if constexpr (CONSTRAIN)
         MakeTypeConstrained();
   }
   
   /// Set the contained data type                                            
   ///   @tparam T - the contained type                                       
   ///   @tparam CONSTRAIN - whether or not to enable type-constraints        
   template<CT::Data T, bool CONSTRAIN, CT::Block THIS> LANGULUS(INLINED)
   void Block::SetType() {
      static_assert(not CT::Typed<THIS>,
         "Can't change type of a statically typed container");
      SetType<CONSTRAIN, THIS>(MetaDataOf<T>());
   }
   
   /// Reset the type of the block, unless it's type-constrained              
   /// Typed THIS makes sure, that this is a no-op                            
   template<CT::Block THIS> LANGULUS(INLINED)
   constexpr void Block::ResetType() noexcept {
      if constexpr (not CT::Typed<THIS>) {
         if (not IsTypeConstrained<THIS>())
            mType = nullptr;
      }
   }

} // namespace Langulus::Anyness