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
   protected:
      using Byte = ::std::uint8_t;
      template<CT::Container C>
      using View = typename C::ViewType;
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();
      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;
      template<CT::Container C>
      using Pick = Tif<CT::Mutable<C>, typename Deref<C>::PickMut, typename Deref<C>::Pick>;

      // The raw pointer                                                
      union {
         char*  mReadableHeap;
         Byte*  mHeap = nullptr;
         void** mSparseHeap;
      };

   public:
      using CTTI_Component = Yes;
      
      /// Get a direct access to the heap memory                              
      template<CT::Container C>
      auto GetRaw(this C&& self) noexcept {
         using T = TypeOf<C>;
         if constexpr (CT::Mutable<C>)
            return reinterpret_cast<      T*>(self.mHeap);
         else
            return reinterpret_cast<const T*>(self.mHeap);
      }

      /// Get a direct access to the heap memory as a different type          
      template<class T, CT::Container C>
      auto GetRawAs(this C&& self) noexcept {
         if constexpr (CT::Mutable<C>)
            return reinterpret_cast<      T*>(self.mHeap);
         else
            return reinterpret_cast<const T*>(self.mHeap);
      }

      /// Get first element pointer or reference, depending on T              
      /// This is a lower-level routine that does only sparseness checking    
      /// No conversion or copying occurs, only pointer arithmetic            
      ///   @attention assumes the container is typed                         
      ///   @tparam T - the type of data we're accessing                      
      ///      use void to use the type of the container, if statically typed 
      template<class T = void, CT::Container C>
      decltype(auto) Get(this C&& self) has_assumptions {
         using DT = Deref<T>;
         using DC = Deref<C>;

         if constexpr (DC::TypeErased) {
            AssumeDev(self.mType, HERE(), "Block is not typed");

            void* pointer;
            if (self.mType.IsSparse())
               pointer = *self.mSparseHeap;
            else
               pointer =  self.mHeap;

            if constexpr (CT::Dense<T>)
               return *reinterpret_cast<DT*>(pointer);
            else
               return  reinterpret_cast<Deptr<DT>*>(pointer);
         }
         else {
            if constexpr (DC::Sparse) {
               if constexpr (CT::Dense<T>)
                  return static_cast<DT&>(**self.mSparseHeap);
               else
                  return static_cast<DT >( *self.mSparseHeap);
            }
            else {
               if constexpr (CT::Dense<T>)
                  return static_cast<DT&>(*self.mHeap);
               else
                  return static_cast<DT >( self.mHeap);
            }
         }
      }
   };

} // namespace Langulus::Anyness::Component
