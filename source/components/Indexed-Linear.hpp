#pragma once
#include "../Container.hpp"
#include <Langulus/CT/Index.hpp>
#include <limits>


namespace Langulus::Anyness::Component
{

   ///                                                                        
   /// Provides random element access based on a linear index, that is        
   /// mapped directly onto contiguous memory                                 
   ///   @tparam T - constrain the type of allowed indices. Leave as 'void'   
   ///      to allow for all the usual integer types                          
   template<class T = void>
   struct IndexedLinear {
      using CTTI_Component = Yes;
      static constexpr bool Indexed = true;

   private:
      template<CT::Container C>
      using Count = typename Deref<C>::CountType;

      template<CT::Container C>
      static constexpr auto CountMax = ::std::numeric_limits<Count<C>>::max();

      template<CT::Container C>
      using Deep = typename Deref<C>::DeepType;

      template<CT::Container C>
      using Pick = Tif<CT::Mutable<C>, typename Deref<C>::PickMut, typename Deref<C>::Pick>;

      template<CT::Container C>
      using PickRange = Tif<CT::Mutable<C>, typename Deref<C>::PickRangeMut, typename Deref<C>::PickRange>;

   public:
      template<CT::Container C>
      auto Get(this C&&, CT::Index auto) has_assumptions -> Pick<C>;

      template<CT::Container C>
      auto GetDeep(this C&&, CT::Index auto) has_assumptions -> Pick<C>;

      template<CT::Container C>
      auto operator[] (this C&&, CT::Index auto) -> Pick<C>;

      template<CT::Container C>
      auto operator * (this C&&) -> Deep<C>;

      template<CT::NotVoid, CT::Container C>
      auto As(this C&&, CT::Index auto) -> Pick<C>;

      template<CT::NotVoid AS, CT::Container C>
      auto As(this C&& self) -> Pick<C> { return self.template As<AS>(0); }

      template<CT::Container C>
      auto Last(this C&&) -> Pick<C>;

      template<CT::NotVoid AS, bool FATAL_FAILURE = true, CT::Container C>
      AS AsCast(this C const&, CT::Index auto);

      template<CT::NotVoid AS, bool FATAL_FAILURE = true, CT::Container C>
      AS AsCast(this C const& self) { return self.template AsCast<AS, FATAL_FAILURE>(0); }
   
      template<CT::Container C>
      auto Select(this C&&, Count<C>, Count<C>) has_assumptions -> PickRange<C>;

      template<CT::Container C>
      auto GetElementDense(this C&&, CT::Index auto, Count<C> = CountMax<C>) -> Deep<C>;
   
      template<CT::Container C>
      auto GetElementDense(this C&&, Count<C> = CountMax<C>) -> Deep<C>;
   
      template<CT::Container C>
      auto GetElementResolved(this C&&, CT::Index auto) -> Deep<C>;

      template<CT::Container C>
      auto GetElementResolved(this C&&) -> Deep<C>;

      template<CT::Container C>
      auto GetElement(this C&&, CT::Index auto) has_assumptions -> Deep<C>;
   
      template<CT::Container C>
      auto GetElement(this C&&) has_assumptions -> Deep<C>;
   
      template<CT::Container C>
      auto GetBlockDeep(this C&&, CT::Index auto) noexcept -> Deep<C>*;
   
      template<CT::Container C>
      auto GetBlockDeep(this C&&) noexcept -> Deep<C>*;
   
      template<CT::Container C>
      auto GetElementDeep(this C&&, CT::Index auto) noexcept -> Deep<C>;

      template<CT::Container C>
      auto GetElementDeep(this C&&) noexcept -> Deep<C>;

      template<CT::Container C>
      auto GetResolved(this C&&) -> Deep<C>;

      template<CT::Container C>
      auto GetDense(this C&&, Count<C> = CountMax<C>) -> Deep<C>;

      template<CT::Container C>
      void SwapIndices(this C&, CT::Index auto, CT::Index auto);

      template<CT::Index, CT::Container C>
      auto GetIndex(this C const&) has_assumptions -> Count<C>;

      template<CT::Container C>
      auto GetIndexMode(this C const&, Count<C>&) has_assumptions -> Count<C>;
   };

} // namespace Langulus::Anyness::Component
