#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Adds a variable to a container that only references a remote heap      
   /// No allocation interface is provided                                    
   /// Increases the container's bytesize                                     
   ///   @tparam ID - multiple references are supported                       
   ///                                                                        
   template<unsigned ID = 0>
   struct HeapReference {
      using CTTI_Component = Yes;
      static constexpr bool HeapAllocated = true;
      static constexpr bool HeapCanBeNull = false;

   protected:
      /*using Byte = ::std::uint8_t;
      template<CT::Container C>
      using View = typename C::ViewType;
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();
      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;
      template<CT::Container C>
      using Pick = Tif<CT::Mutable<C>, typename Deref<C>::PickMut, typename Deref<C>::Pick>;*/

      // The raw pointer                                                
      union {
         char*  mReadableHeap;
         void*  mHeap = nullptr;
         void** mSparseHeap;
      };

   public:
      /// A heap reference can not be default-initialized to a nullptr        
      /// It must always reference a valid heap allocation                    
      HeapReference() = delete;

      constexpr HeapReference(void* heap) noexcept
         : mHeap {heap} {}

      /// Get a direct access to the heap memory                              
      template<CT::Container C>
      constexpr auto GetRaw(this C&& self) noexcept {
         using T = TypeOf<C>;
         if constexpr (CT::Mutable<C>)
            return static_cast<      T*>(self.mHeap);
         else
            return static_cast<const T*>(self.mHeap);
      }

      /// Get a direct access to the heap memory as a different type          
      template<class T, CT::Container C>
      constexpr auto GetRawAs(this C&& self) noexcept {
         if constexpr (CT::Mutable<C>)
            return static_cast<      T*>(self.mHeap);
         else
            return static_cast<const T*>(self.mHeap);
      }
      
      /// Get first element pointer or reference, depending on T              
      /// This is a lower-level routine that does only sparseness checking    
      /// No conversion or copying occurs, only pointer arithmetic            
      ///   @attention assumes the container is typed                         
      ///   @tparam T - the type of data we're accessing                      
      ///      use void to use the type of the container, if statically typed 
      template<class T = void, CT::Container C>
      constexpr auto& Get(this C&& self) has_assumptions {
         static_assert(not CT::Handle<T>, "T can't be a handle");
         static_assert(not CT::Reference<T>, "Strip references");
         using DC = Deref<C>;
         using TT = DecvqAll<Tif<CT::Void<T>, TypeOf<C>, T>>;

         if constexpr (CT::Void<TT>) {
            // Type-erased reference, no casting                        
            if (self.IsSparse())
               return static_cast<void**&>(self.mHeap);
            else
               return static_cast<void* &>(self.mHeap);
         }
         else if constexpr (DC::TypeErased) {
            // Casting to a desired runtime type                        
            AssumeDev(self.IsTyped(), HERE(), "Block is not typed");

            if (self.IsSparse()) {
               if constexpr (CT::Dense<TT>)
                  return **static_cast<TT**>(self.mHeap);
               else
                  return  *static_cast<TT* >(self.mHeap);
            }
            else {
               if constexpr (CT::Dense<TT>)
                  return *static_cast<TT*>( self.mHeap);
               else
                  return *static_cast<TT*>(&self.mHeap);
            }
         }
         else {
            // Casting to a desired static type                         
            if constexpr (DC::Sparse) {
               if constexpr (CT::Dense<TT>)
                  return **static_cast<TT**>(self.mHeap);
               else
                  return  *static_cast<TT* >(self.mHeap);
            }
            else {
               if constexpr (CT::Dense<TT>)
                  return *static_cast<TT*>( self.mHeap);
               else
                  return *static_cast<TT*>(&self.mHeap);
            }
         }
      }
   };

} // namespace Langulus::Anyness::Component
