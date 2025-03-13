#pragma once
#include "../Container.hpp"


namespace Langulus::Anyness::Component
{
   
   ///                                                                        
   /// Implements insertion for containers                                    
   ///   @tparam AS - type to serialize as before inserting. Useful for byte  
   ///      and text containers. Use void to insert without serialization     
   struct Concatenate {
      using CTTI_Component = Yes;

      template<CT::Container C>
      static consteval bool Validate() {
         static_assert(C::VariableCount, "You can't concatenate stuff in a "
            "container that doesn't provide variable count");
      }

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Deep = typename C::DeepType;

   public:
      /// Concatenation at specific index                                     
      template<bool FORCE = true, CT::Container C>
      auto ConcatAt(this C&, CT::Index auto, CT::Container auto&&)
         -> Count<C> requires C::Indexed;

      /// Generic concatenation                                               
      template<bool FORCE = true, CT::Container C>
      auto Concat(this C&, CT::Container auto&&)
         -> Count<C>;
   };

} // namespace Langulus::Anyness::Component
