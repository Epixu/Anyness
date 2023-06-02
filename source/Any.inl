///                                                                           
/// Langulus::Anyness                                                         
/// Copyright(C) 2012 Dimo Markov <langulusteam@gmail.com>                    
///                                                                           
/// Distributed under GNU General Public License v3+                          
/// See LICENSE file, or https://www.gnu.org/licenses                         
///                                                                           
#pragma once
#include "Any.hpp"
#include <utility>

namespace Langulus::Anyness
{

   /// Copy constructor - does only a shallow copy                            
   ///   @param other - the container to shallow-copy                         
   LANGULUS(INLINED)
   Any::Any(const Any& other)
      : Any {Copy(other)} {}

   /// Move constructor - transfers ownership                                 
   ///   @param other - the container to move                                 
   LANGULUS(INLINED)
   Any::Any(Any&& other) noexcept
      : Any {Move(other)} {}

   /// Construct by shallow-copying element/container                         
   ///   @param other - the element/container to shallow-copy                 
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any::Any(const T& other)
      : Any {Copy(other)} {}

   /// Construct by shallow-copying element/container                         
   ///   @param other - the element/container to shallow-copy                 
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any::Any(T& other)
      : Any {Copy(other)} {}

   /// Construct by moving element/container                                  
   ///   @param other - the element/container to move                         
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any::Any(T&& other) requires CT::Mutable<T>
      : Any {Move(other)} {}

   /// Semantic constructor from deep container or custom data element        
   ///   @tparam S - type of insertion and semantic to use (deducible)        
   ///   @param other - the element/container to initialize with              
   template<CT::Semantic S>
   LANGULUS(INLINED)
   Any::Any(S&& other) noexcept {
      using T = TypeOf<S>;

      if constexpr (CT::Deep<T>) {
         // Copy/Disown/Move/Abandon/Clone a deep container             
         BlockTransfer<Any>(other.Forward());
      }
      else if constexpr (CT::CustomData<T>) {
         if constexpr (CT::Array<T>) {
            // Copy/Disown/Move/Abandon/Clone an array of elements      
            SetType<Deext<T>, false>();
            AllocateFresh(RequestSize(ExtentOf<T>));
            Offset offset {};
            for (auto& element : *other)
               InsertInner(S::Nest(element), offset++);
         }
         else {
            // Copy/Disown/Move/Abandon/Clone a single element          
            SetType<T, false>();
            AllocateFresh(RequestSize(1));
            InsertInner(other.Forward(), 0);
         }
      }
      else LANGULUS_ERROR("Bad semantic constructor argument");
   }

   /// Pack any number of elements sequentially                               
   /// If any of the types doesn't match exactly, the container becomes deep  
   /// to incorporate all elements                                            
   ///   @param head - first element                                          
   ///   @param tail... - the rest of the elements                            
   template<CT::Data HEAD, CT::Data... TAIL>
   Any::Any(HEAD&& head, TAIL&&... tail) requires (sizeof...(TAIL) >= 1) {
      if constexpr (CT::Semantic<HEAD>) {
         // Types differ, so wrap each of them in a separate Any        
         SetType<Any, false>();
         AllocateFresh(RequestSize(sizeof...(TAIL) + 1));

         InsertInner(Abandon(Any {head.Forward()}), 0);
         InsertStatic<1>(Abandon(Any {Forward<TAIL>(tail)})...);
      }
      else if constexpr (CT::Exact<HEAD, TAIL...>) {
         // All types are the same, so pack them tightly                
         SetType<Decvq<Deref<HEAD>>, false>();
         AllocateFresh(RequestSize(sizeof...(TAIL) + 1));

         if constexpr (::std::is_rvalue_reference_v<HEAD>)
            InsertInner(Langulus::Move(head), 0);
         else
            InsertInner(Langulus::Copy(head), 0);

         InsertStatic<1>(Forward<TAIL>(tail)...);
      }
      else {
         // Types differ, so wrap each of them in a separate Any        
         SetType<Any, false>();
         AllocateFresh(RequestSize(sizeof...(TAIL) + 1));

         InsertInner(Abandon(Any {Forward<HEAD>(head)}), 0);
         InsertStatic<1>(Abandon(Any {Forward<TAIL>(tail)})...);
      }
   }

   /// Destruction                                                            
   LANGULUS(INLINED)
   Any::~Any() {
      Free();
   }

   /// Create an empty Any from a dynamic type and state                      
   ///   @param type - type of the container                                  
   ///   @param state - optional state of the container                       
   ///   @return the new container instance                                   
   LANGULUS(INLINED)
   Any Any::FromMeta(DMeta type, const DataState& state) noexcept {
      return Any {Block {state, type}};
   }

   /// Create an empty Any by copying type and state of a block               
   ///   @param block - the source of type and state                          
   ///   @param state - additional state of the container                     
   ///   @return the new container instance                                   
   LANGULUS(INLINED)
   Any Any::FromBlock(const Block& block, const DataState& state) noexcept {
      return Any::FromMeta(block.GetType(), block.GetUnconstrainedState() + state);
   }

   /// Create an empty Any by copying only state of a block                   
   ///   @param block - the source of the state                               
   ///   @param state - additional state of the container                     
   ///   @return the new container instance                                   
   LANGULUS(INLINED)
   Any Any::FromState(const Block& block, const DataState& state) noexcept {
      return Any::FromMeta(nullptr, block.GetUnconstrainedState() + state);
   }

   /// Create an empty Any from a static type and state                       
   ///   @tparam T - the contained type                                       
   ///   @param state - optional state of the container                       
   ///   @return the new container instance                                   
   template<CT::Data T>
   LANGULUS(INLINED)
   Any Any::From(const DataState& state) noexcept {
      return Block {state, MetaData::Of<T>()};
   }

   /// Pack any number of similarly typed elements sequentially               
   ///   @tparam AS - the type to wrap elements as                            
   ///                use 'void' to deduce AS from the HEAD                   
   ///                (void by default)                                       
   ///   @tparam HEAD - the first element type (deducible)                    
   ///   @tparam TAIL... - the rest of the element types (deducible)          
   ///   @param head - first element                                          
   ///   @param tail... - the rest of the elements                            
   ///   @returns the new container containing the data                       
   template<class AS, CT::Data HEAD, CT::Data... TAIL>
   LANGULUS(INLINED)
   Any Any::WrapAs(HEAD&& head, TAIL&&... tail) {
      if constexpr (sizeof...(TAIL) == 0)
         return {};
      else if constexpr (CT::Void<AS>) {
         static_assert(CT::Exact<HEAD, TAIL...>, "Type mismatch");
         return {Forward<HEAD>(head), Forward<HEAD>(tail)...};
      }
      else {
         static_assert(CT::DerivedFrom<HEAD, AS>, "Head not related");
         static_assert((CT::DerivedFrom<TAIL, AS> && ...), "Tail not related");
         return {Forward<AS>(head), Forward<AS>(tail)...};
      }
   }
   
   /// Shallow-copy assignment                                                
   ///   @param other - the container to copy                                 
   ///   @return a reference to this container                                
   LANGULUS(INLINED)
   Any& Any::operator = (const Any& other) {
      return operator = (Copy(other));
   }

   /// Move assignment                                                        
   ///   @param other - the container to move and reset                       
   ///   @return a reference to this container                                
   LANGULUS(INLINED)
   Any& Any::operator = (Any&& other) noexcept {
      return operator = (Move(other));
   }

   /// Shallow copy assignment of anything                                    
   ///   @tparam T - the type to copy (deducible)                             
   ///   @param other - the value to copy                                     
   ///   @return a reference to this container                                
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator = (const T& other) {
      return operator = (Copy(other));
   }
   
   /// Shallow copy assignment of anything                                    
   ///   @tparam T - the type to copy (deducible)                             
   ///   @param other - the value to copy                                     
   ///   @return a reference to this container                                
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator = (T& other) {
      return operator = (Copy(other));
   }

   /// Move assignment of anything                                            
   ///   @tparam T - the type to move in (deducible)                          
   ///   @param other - the value to move in                                  
   ///   @return a reference to this container                                
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator = (T&& other) requires CT::Mutable<T> {
      return operator = (Move(other));
   }

   /// Semantic assignment                                                    
   ///   @tparam S - the semantic and type to assign (deducible)              
   ///   @param other - the container to semantically assign                  
   ///   @return a reference to this container                                
   template<CT::Semantic S>
   Any& Any::operator = (S&& other) {
      using T = TypeOf<S>;
      static_assert(CT::Insertable<T>, "T must be an insertable type");

      if constexpr (CT::Deep<T>) {
         // Assign a container                                          
         if (this == &*other)
            return *this;

         // Since Any is type-erased, we make a runtime type check      
         LANGULUS_ASSERT(!IsTypeConstrained() || CastsToMeta(other->GetType()),
            Assign, "Incompatible types");

         Free();
         new (this) Any {other.Forward()};
      }
      else {
         // Assign a non-deep value                                     
         const auto meta = MetaData::Of<T>();

         LANGULUS_ASSERT(!IsTypeConstrained() || CastsToMeta(meta),
            Assign, "Incompatible types");

         if (GetUses() != 1 || mType->mIsSparse != CT::Sparse<T>) {
            // Reset and allocate fresh memory                          
            Reset();
            operator << (other.Forward());
         }
         else{
            // Just destroy and reuse memory                            
            CallKnownDestructors<T>();
            mCount = 1;
            SemanticNew<T>(mRaw, other.Forward());
         }
      }

      return *this;
   }

   /// Destroy all elements, but retain allocated memory if possible          
   LANGULUS(INLINED)
   void Any::Clear() {
      if (IsEmpty())
         return;

      if (GetUses() == 1) {
         // Only one use - just destroy elements and reset count,       
         // reusing the allocation for later                            
         CallUnknownDestructors();
         ClearInner();
      }
      else {
         // We're forced to reset the memory, because it's in use       
         // Keep the type and state, though                             
         const auto state = GetUnconstrainedState();
         const auto meta = mType;
         Reset();
         mType = meta;
         mState += state;
      }
   }

   /// Copy-insert an element (including arrays) at the back                  
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator << (const T& other) {
      Insert<IndexBack>(Copy(other));
      return *this;
   }

   /// Used to disambiguate from the && variant                               
   /// Dimo, I know you want to remove this, but don't, said Dimo to himself  
   /// after actually deleting this function numerous times                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator << (T& other) {
      Insert<IndexBack>(Copy(other));
      return *this;
   }

   /// Move-insert an element at the back                                     
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator << (T&& other) {
      Insert<IndexBack>(Move(other));
      return *this;
   }

   /// Move-insert an element at the back                                     
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::Semantic S>
   LANGULUS(INLINED)
   Any& Any::operator << (S&& other) {
      Insert<IndexBack>(other.Forward());
      return *this;
   }

   /// Copy-insert an element (including arrays) at the front                 
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator >> (const T& other) {
      Insert<IndexFront>(Copy(other));
      return *this;
   }

   /// Used to disambiguate from the && variant                               
   /// Dimo, I know you want to remove this, but don't, said Dimo to himself  
   /// after actually deleting this function numerous times                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator >> (T& other) {
      Insert<IndexFront>(Copy(other));
      return *this;
   }

   /// Move-insert element at the front                                       
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator >> (T&& other) {
      Insert<IndexFront>(Move(other));
      return *this;
   }
   
   /// Move-insert element at the front                                       
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::Semantic S>
   LANGULUS(INLINED)
   Any& Any::operator >> (S&& other) {
      Insert<IndexFront>(other.Forward());
      return *this;
   }

   /// Merge data (including arrays) at the back                              
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator <<= (const T& other) {
      Merge<IndexBack, true>(Copy(other));
      return *this;
   }

   /// Used to disambiguate from the && variant                               
   /// Dimo, I know you want to remove this, but don't, said Dimo to himself  
   /// after actually deleting this function numerous times                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator <<= (T& other) {
      Merge<IndexBack, true>(Copy(other));
      return *this;
   }

   /// Merge data at the back by move-insertion                               
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator <<= (T&& other) {
      Merge<IndexBack, true>(Move(other));
      return *this;
   }

   /// Merge data at the back by move-insertion                               
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::Semantic S>
   LANGULUS(INLINED)
   Any& Any::operator <<= (S&& other) {
      Merge<IndexBack, true>(other.Forward());
      return *this;
   }

   /// Merge data at the front                                                
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator >>= (const T& other) {
      Merge<IndexFront, true>(Copy(other));
      return *this;
   }

   /// Used to disambiguate from the && variant                               
   /// Dimo, I know you want to remove this, but don't, said Dimo to himself  
   /// after actually deleting this function numerous times                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator >>= (T& other) {
      Merge<IndexFront, true>(Copy(other));
      return *this;
   }

   /// Merge data at the front by move-insertion                              
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::NotSemantic T>
   LANGULUS(INLINED)
   Any& Any::operator >>= (T&& other) {
      Merge<IndexFront, true>(Move(other));
      return *this;
   }

   /// Merge data at the front by move-insertion                              
   ///   @param other - the data to insert                                    
   ///   @return a reference to this container for chaining                   
   template<CT::Semantic S>
   LANGULUS(INLINED)
   Any& Any::operator >>= (S&& other) {
      Merge<IndexFront, true>(other.Forward());
      return *this;
   }

   /// Reset the container                                                    
   LANGULUS(INLINED)
   void Any::Reset() {
      Free();
      mRaw = nullptr;
      mCount = mReserved = 0;
      ResetState();
   }

   /// Swap two container's contents                                          
   ///   @param other - [in/out] the container to swap contents with          
   LANGULUS(INLINED)
   void Any::Swap(Any& other) noexcept {
      other = ::std::exchange(*this, ::std::move(other));
   }

   /// Pick a constant region and reference it from another container         
   ///   @param start - starting element index                                
   ///   @param count - number of elements                                    
   ///   @return the container                                                
   LANGULUS(INLINED)
   Any Any::Crop(const Offset& start, const Count& count) const {
      return Any {Block::Crop(start, count)};
   }

   /// Pick a region and reference it from another container                  
   ///   @param start - starting element index                                
   ///   @param count - number of elements                                    
   ///   @return the container                                                
   LANGULUS(INLINED)
   Any Any::Crop(const Offset& start, const Count& count) {
      return Any {Block::Crop(start, count)};
   }




   ///                                                                        
   ///   Concatenation                                                        
   ///                                                                        

   /// An inner concatenation routine using move/abandon                      
   ///   @tparam WRAPPER - the type of the concatenated container             
   ///   @tparam S - block type and semantic to concatenate with (deducible)  
   ///   @param rhs - block to concatenate                                    
   ///   @return the concatenated container                                   
   template<CT::Block WRAPPER, CT::Semantic S>
   WRAPPER Any::Concatenate(S&& rhs) const {
      static_assert(CT::Block<TypeOf<S>>,
         "S type must be a block type");

      if (IsEmpty())
         return {rhs.Forward()};
      else if (rhs->IsEmpty())
         return reinterpret_cast<const WRAPPER&>(*this);

      WRAPPER result;
      if constexpr (!CT::Typed<WRAPPER>)
         result.template SetType<false>(mType);

      result.AllocateFresh(result.RequestSize(mCount + rhs->mCount));
      result.InsertBlock(reinterpret_cast<const WRAPPER&>(*this));
      result.InsertBlock(rhs.Forward());
      return Abandon(result);
   }

   /// Copy-concatenate with any deep type                                    
   ///   @tparam T - type of the container to concatenate (deducible)         
   ///   @param rhs - the right operand                                       
   ///   @return the combined container                                       
   template<CT::Deep T>
   LANGULUS(INLINED)
   Any Any::operator + (const T& rhs) const requires CT::Dense<T> {
      return Concatenate<Any>(Copy(rhs));
   }

   template<CT::Deep T>
   LANGULUS(INLINED)
   Any Any::operator + (T& rhs) const requires CT::Dense<T> {
      return Concatenate<Any>(Copy(rhs));
   }

   /// Move-concatenate with any deep type                                    
   ///   @tparam T - type of the container to concatenate (deducible)         
   ///   @param rhs - the right operand                                       
   ///   @return the combined container                                       
   template<CT::Deep T>
   LANGULUS(INLINED)
   Any Any::operator + (T&& rhs) const requires CT::Dense<T> {
      return Concatenate<Any>(Move(rhs));
   }

   /// Move-concatenate with any deep type                                    
   ///   @tparam T - type of the container to concatenate (deducible)         
   ///   @param rhs - the right operand                                       
   ///   @return the combined container                                       
   template<CT::Semantic S>
   LANGULUS(INLINED)
   Any Any::operator + (S&& rhs) const requires (CT::Deep<TypeOf<S>>&& CT::Dense<TypeOf<S>>) {
      return Concatenate<Any>(rhs.Forward());
   }

   /// Destructive copy-concatenate with any deep type                        
   ///   @tparam T - type of the container to concatenate (deducible)         
   ///   @param rhs - the right operand                                       
   ///   @return a reference to this modified container                       
   template<CT::Deep T>
   LANGULUS(INLINED)
   Any& Any::operator += (const T& rhs) requires CT::Dense<T> {
      InsertBlock(Copy(rhs));
      return *this;
   }

   template<CT::Deep T>
   LANGULUS(INLINED)
   Any& Any::operator += (T& rhs) requires CT::Dense<T> {
      InsertBlock(Copy(rhs));
      return *this;
   }

   /// Destructive move-concatenate with any deep type                        
   ///   @tparam T - type of the container to concatenate (deducible)         
   ///   @param rhs - the right operand                                       
   ///   @return a reference to this modified container                       
   template<CT::Deep T>
   LANGULUS(INLINED)
   Any& Any::operator += (T&& rhs) requires CT::Dense<T> {
      InsertBlock(Move(rhs));
      return *this;
   }

   /// Destructive move-concatenate with any deep type                        
   ///   @tparam T - type of the container to concatenate (deducible)         
   ///   @param rhs - the right operand                                       
   ///   @return a reference to this modified container                       
   template<CT::Semantic S>
   LANGULUS(INLINED)
   Any& Any::operator += (S&& rhs) requires (CT::Deep<TypeOf<S>>&& CT::Dense<TypeOf<S>>) {
      InsertBlock(rhs.Forward());
      return *this;
   }
   
   /// Find element(s) index inside container                                 
   ///   @tparam REVERSE - true to perform search in reverse                  
   ///   @tparam BY_ADDRESS_ONLY - true to compare addresses only             
   ///   @param item - the item to search for                                 
   ///   @return the index of the found item, or IndexNone if none found      
   template<bool REVERSE, CT::Data T>
   LANGULUS(INLINED)
   Index Any::Find(const T& item, const Offset& cookie) const {
      return Block::template FindKnown<REVERSE>(item, cookie);
   }


   ///                                                                        
   ///   Iteration                                                            
   ///                                                                        

   /// Get iterator to first element                                          
   ///   @return an iterator to the first element, or end if empty            
   LANGULUS(INLINED)
   typename Any::Iterator Any::begin() noexcept {
      return IsEmpty() ? end() : GetElement();
   }

   /// Get iterator to end                                                    
   ///   @return an iterator to the end element                               
   LANGULUS(INLINED)
   typename Any::Iterator Any::end() noexcept {
      Block result {*this};
      if (IsEmpty())
         return result;

      result.MakeStatic();
      result.mRaw = mRaw + mType->mSize * mCount;
      result.mCount = 0;
      return result;
   }

   /// Get iterator to the last element                                       
   ///   @return an iterator to the last element, or end if empty             
   LANGULUS(INLINED)
   typename Any::Iterator Any::last() noexcept {
      Block result {*this};
      if (IsEmpty())
         return result;

      result.MakeStatic();
      result.mRaw = mRaw + mType->mSize * (mCount - 1);
      result.mCount = 1;
      return result;
   }

   /// Get iterator to first element                                          
   ///   @return a constant iterator to the first element, or end if empty    
   LANGULUS(INLINED)
   typename Any::ConstIterator Any::begin() const noexcept {
      return IsEmpty() ? end() : GetElement();
   }

   /// Get iterator to end                                                    
   ///   @return a constant iterator to the end element                       
   LANGULUS(INLINED)
   typename Any::ConstIterator Any::end() const noexcept {
      Block result {*this};
      if (IsEmpty())
         return result;

      result.MakeStatic();
      result.mRaw = mRaw + mType->mSize * mCount;
      result.mCount = 0;
      return result;
   }

   /// Get iterator to the last valid element                                 
   ///   @return a constant iterator to the last element, or end if empty     
   LANGULUS(INLINED)
   typename Any::ConstIterator Any::last() const noexcept {
      Block result {*this};
      if (IsEmpty())
         return result;

      result.MakeStatic();
      result.mRaw = mRaw + mType->mSize * (mCount - 1);
      result.mCount = 1;
      return result;
   }
   
   /// Construct an item of this container's type at the specified position   
   /// by forwarding A... as constructor arguments                            
   /// Since this container is type-erased and exact constructor signatures   
   /// aren't reflected, the following constructors will be attempted:        
   ///   1. If A is a single argument of exactly the same type, the reflected 
   ///      move constructor will be used, if available                       
   ///   2. If A is empty, the reflected default constructor is used          
   ///   3. If A is not empty, not exactly same as the contained type, or     
   ///      is more than a single argument, then all arguments will be        
   ///      wrapped in an Any, and then forwarded to the descriptor-          
   ///      constructor, if such is reflected                                 
   ///   If none of these constructors are available, this function throws    
   ///   Except::Construct                                                    
   ///   @tparam A... - argument types (deducible)                            
   ///   @param region - the region to emplace at                             
   ///   @param count - the number of elements to emplace                     
   ///   @param arguments... - the arguments to forward to constructor        
   ///   @return the number of emplaced elements                              
   template<class... A>
   void Block::EmplaceInner(const Block& region, Count count, A&&... arguments) {
      if constexpr (sizeof...(A) == 0) {
         // Attempt default construction                                
         //TODO if stuff moved, we should move stuff back if this throws...
         region.CallUnknownDefaultConstructors(count);
      }
      else if constexpr (sizeof...(A) == 1) {
         if constexpr (CT::Exact<A..., Descriptor>) {
            // Attempt descriptor-construction                          
            region.CallUnknownDescriptorConstructors(count, arguments...);
         }
         else {
            // Attempt move-construction, if available                  
            using F = Decvq<Deref<typename TTypeList<A...>::First>>;
            if (IsExact<F>()) {
               // Single argument matches                               
               region.template CallKnownConstructors<F>(
                  count, Forward<A>(arguments)...
               );

               mCount += count;
               return;
            }
         }
      }

      LANGULUS_ASSERT(
         mType->mDefaultConstructor != nullptr, Construct,
         "Can't descriptor-construct element"
         " - no descriptor-constructor reflected"
      );

      // Attempt wrapping argument(s) in a Descriptor, and doing        
      // descriptor-construction, if available                          
      //TODO if stuff moved, we should move stuff back if this throws...
      Descriptor descriptor {Block::From(arguments)...};
      region.CallUnknownDescriptorConstructors(count, descriptor);
      mCount += count;
   }
   

   ///                                                                        
   ///   Block iterator                                                       
   ///                                                                        

   /// Construct an iterator                                                  
   ///   @param value - pointer to the value element                          
   template<bool MUTABLE>
   LANGULUS(INLINED)
   Any::TIterator<MUTABLE>::TIterator(const Block& value) noexcept
      : mValue {value} {}

   /// Prefix increment operator                                              
   ///   @attention assumes iterator points to a valid element                
   ///   @return the modified iterator                                        
   template<bool MUTABLE>
   LANGULUS(INLINED)
   typename Any::TIterator<MUTABLE>& Any::TIterator<MUTABLE>::operator ++ () noexcept {
      mValue.mRaw += mValue.GetStride();
      return *this;
   }

   /// Suffix increment operator                                              
   ///   @attention assumes iterator points to a valid element                
   ///   @return the previous value of the iterator                           
   template<bool MUTABLE>
   LANGULUS(INLINED)
   typename Any::TIterator<MUTABLE> Any::TIterator<MUTABLE>::operator ++ (int) noexcept {
      const auto backup = *this;
      operator ++ ();
      return backup;
   }

   /// Compare block entries                                                  
   ///   @param rhs - the other iterator                                      
   ///   @return true if entries match                                        
   template<bool MUTABLE>
   LANGULUS(INLINED)
   bool Any::TIterator<MUTABLE>::operator == (const TIterator& rhs) const noexcept {
      return mValue.mRaw == rhs.mValue.mRaw;
   }

   /// Iterator access operator                                               
   ///   @return a pair at the current iterator position                      
   template<bool MUTABLE>
   LANGULUS(INLINED)
   const Block& Any::TIterator<MUTABLE>::operator * () const noexcept {
      return mValue;
   }

} // namespace Langulus::Anyness
