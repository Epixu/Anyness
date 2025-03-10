#pragma once
#include "../Container.hpp"
#include <Langulus/Intent.hpp>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Implements removal for containers                                      
   ///                                                                        
   struct Removal {
      using CTTI_Component = Yes;

      template<CT::Container C>
      static consteval bool Validate() {
         static_assert(C::VariableCount, "You can't remove stuff in a "
            "container that doesn't provide variable count");
      }

   private:
      template<CT::Container C>
      using Count = typename C::CountType;
      template<CT::Container C>
      using Iterator = typename C::Iterator;

   public:
      template<bool REVERSE = false, CT::Container C>
      auto Remove(this C&, const CT::NoIntent auto&) -> Count<C>;

      template<CT::Container C>
      auto RemoveAt(this C&, CT::Index auto, Count<C> = 1) -> Count<C>;

      template<CT::Container C>
      auto RemoveAtDeep(CT::Index auto) -> Count<C>;

      template<CT::Container C>
      auto RemoveIt(this C&, const Iterator<C>&, Count<C> = 1) -> Iterator<C>;

      template<CT::Container C>
      void Trim(this C&, Count<C>);

      template<CT::Container C>
      void Optimize(this C&);

      template<CT::Container C>
      void Clear(this C&);

      template<CT::Container C>
      void Reset(this C&);
   };

} // namespace Langulus::Anyness::Component
